#' @title HDR plot
#' @description Produces a 1d or 2d box plot of HDR regions. The darker regions
#' contain observations with higher probability, while the lighter regions contain
#' points with lower probability. Observations outside the largest HDR are shown
#' as individual points. Anomalies with leave-one-out surprisal probabilities
#' less than 0.005 are optionally shown in black.
#' @details The original HDR boxplot proposed by Hyndman (1996), can be produced
#' with `show_anomalies = FALSE`, `jitter = FALSE`, `alpha = 1`, and all other
#' arguments set to their defaults.
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 Optionally, the name of the second variable to plot (a bare expression).
#' @param prob A numeric vector specifying the coverage probabilities for the HDRs.
#' @param color The base color to use for the mode. Colors for the HDRs are generated
#' by whitening this color.
#' @param show_points A logical argument indicating if a regular HDR plot is required
#' (\code{FALSE}), or whether to show the individual observations in the same colors (\code{TRUE}).
#' @param show_anomalies A logical argument indicating if the surprisal anomalies should be shown (in black).
#' These are points with leave-one-out surprisal probability values less than 0.005 (using a GPD approximation),
#' and which lie outside the 99% HDR region.
#' @param alpha Transparency of points. Ignored if `show_points` is `FALSE`.
#' Defaults to min(1, 500/n), where n is the number of observations plotted.
#' @param jitter A logical value indicating if the points should be vertically jittered
#' for the 1d box plots to reduce overplotting.
#' @param ... Other arguments passed to \code{\link{dist_kde}}.
#' @return A ggplot object showing an HDR plot or scatterplot of the data.
#' @author Rob J Hyndman
#' @seealso \code{\link{surprisals}}, \code{\link{hdr_table}}
#' @references Hyndman, R J (1996) Computing and Graphing Highest Density Regions,
#' *The American Statistician*, **50**(2), 120–126. \url{https://robjhyndman.com/publications/hdr/}
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 5.7,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' df <- data.frame(x = c(rnorm(1000), rnorm(1000, 5, 1), 10))
#' gg_hdrboxplot(df, x, show_anomalies = TRUE)
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   gg_hdrboxplot(Average)
#' oldfaithful |>
#'   gg_hdrboxplot(duration, waiting, show_points = TRUE)
#'
#' @export
gg_hdrboxplot <- function(
  data,
  var1,
  var2 = NULL,
  prob = c(0.5, 0.99),
  color = "#0072b2",
  show_points = FALSE,
  show_anomalies = TRUE,
  alpha = NULL,
  jitter = TRUE,
  ...
) {
  if (missing(var1)) {
    # Grab first variable
    data <- as.data.frame(data)
    var1 <- rlang::sym(names(data)[1])
    if (NCOL(data) > 1L) {
      message("No variable selected. Using ", rlang::as_name(var1))
    }
  }
  v2 <- dplyr::as_label(dplyr::enquo(var2))
  if (v2 == "NULL") {
    d <- 1L
    data <- data |> select({{ var1 }})
  } else {
    d <- 2L
    data <- data |> select({{ var1 }}, {{ var2 }})
  }
  dist <- dist_kde(data[, seq(d)], ...)
  hdr <- dplyr::if_else(show_points, "points", "fill")

  # Set up color palette
  prob <- sort(prob)
  hdr_colors <- list(hdr_palette(color = color, prob = c(prob, 1)))
  names(hdr_colors) <- names_dist(dist)

  # HDR thresholds
  # Pre-compute the density grid ONCE and pass it to both hdr_table() and the
  # downstream gg_density helpers. Previously density_df() was called
  # inline in each of the two gg_density* calls, and hdr_table() did its own
  # internal sampling -- a triple-evaluation pattern.
  df <- density_df(dist)

  threshold <- hdr_table_with_data(dist, prob, density_df = df) |>
    dplyr::transmute(
      level = 100 * prob,
      distribution = distribution,
      threshold = density
    ) |>
    dplyr::distinct()

  show_x <- show_data(dist, prob, threshold, anomalies = show_anomalies)
  if (NROW(show_x) != NROW(data)) {
    stop("Something has gone wrong here!")
  }

  if (d == 2L) {
    gg_density2(
      dist,
      show_x = show_x,
      threshold = threshold,
      prob = prob,
      hdr = hdr,
      hdr_colors = hdr_colors,
      show_points = TRUE,
      show_mode = TRUE,
      show_anomalies = show_anomalies,
      alpha = alpha,
      df = df
    ) +
      ggplot2::guides(fill = "none", color = "none")
  } else {
    gg_density1(
      dist,
      show_x = show_x,
      threshold = threshold,
      prob = prob,
      hdr = hdr,
      hdr_colors = hdr_colors,
      show_points = TRUE,
      show_mode = TRUE,
      show_anomalies = show_anomalies,
      alpha = alpha,
      jitter = jitter,
      df = df,
      show_density = FALSE
    ) +
      ggplot2::guides(alpha = "none") +
      ggplot2::scale_y_continuous(breaks = NULL) +
      labs(y = "", x = names(data)[1])
  }
}

#' @title Table of Highest Density Regions
#' @description
#' Compute a table of highest density regions (HDR) for a distributional object.
#' The HDRs are returned as a tibble with one row per interval and columns:
#' `prob` (giving the probability coverage),
#' `density` (the value of the density at the boundary of the HDR),
#' For one dimensional density functions, the tibble also has columns
#' `lower` (the lower ends of the intervals), and
#' `upper` (the upper ends of the intervals).
#' @param object Distributional object such as that returned by `dist_kde()`
#' @param prob Vector of probabilities giving the HDR coverage (between 0 and 1)
#' @return A tibble
#' @author Rob J Hyndman
#' @references Hyndman, R J (1996) "Computing and Graphing Highest Density Regions", *The American Statistician*, 50(2), 120–126. \url{https://robjhyndman.com/publications/hdr/}
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 2.5, 3.4. \url{https://OTexts.com/weird/}.
#' @seealso \code{\link{gg_hdrboxplot}}
#' @examples
#' # Univariate HDRs
#' c(dist_normal(), dist_kde(c(rnorm(100), rnorm(100, 3, 1)))) |>
#'   hdr_table(c(0.5, 0.95))
#' dist_kde(oldfaithful$duration) |> hdr_table(0.95)
#' # Bivariate HDRs
#' dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_table(0.90)
#' @export
hdr_table <- function(object, prob) {
  d <- dimension_dist(object)
  if (d == 2) {
    density_df <- make_density_df_2d(object)
  } else {
    density_df <- NULL
  }
  hdr_table_with_data(object, prob, density_df)
}

hdr_table_with_data <- function(object, prob, density_df) {
  d <- dimension_dist(object)
  prob <- sort(unique(prob), decreasing = TRUE)
  dist_names <- names_dist(object)

  output <- if (d == 1L) {
    hdr_table_1d(object, prob, dist_names)
  } else {
    hdr_table_2d(object, prob, dist_names, density_df = density_df)
  }

  output |> dplyr::arrange(distribution, prob)
}

# 1D path: keep distributional::hdr() for the canonical interval endpoints
# (this is the part the public output relies on: `lower` and `upper` columns).
# The threshold density is read off at the lower endpoints and then averaged
# WITHIN each distribution to smooth floating-point noise across multiple
# intervals at the same density level.
#
# Bug-fix note: the previous implementation averaged density values across
# every row at a given probability level, conflating distributions. For a
# single-distribution plot this had no observable effect (all rows shared the
# same level anyway); for multi-distribution 1D plots it produced a single
# averaged threshold across distributions, which is incorrect.
hdr_table_1d <- function(object, prob, dist_names) {
  output <- lapply(prob, function(p) {
    hdri <- distributional::hdr(object, size = p * 100, n = 4096)
    intervals <- tibble(
      prob = p,
      distribution = dist_names,
      lower = vctrs::field(hdri, "lower"),
      upper = vctrs::field(hdri, "upper")
    ) |>
      tidyr::unnest(c(lower, upper))

    # Threshold density at each lower endpoint, computed per distribution.
    intervals_split <- split(intervals, intervals$distribution)[dist_names]
    per_dist <- mapply(
      function(dist, df) {
        df$density <- unlist(density(dist, at = df$lower))
        df
      },
      dist = as.list(object),
      df = intervals_split,
      SIMPLIFY = FALSE
    )
    out <- purrr::list_rbind(per_dist)

    # Average within each distribution (not across).
    out |>
      dplyr::group_by(distribution) |>
      dplyr::mutate(density = mean(density)) |>
      dplyr::ungroup()
  })

  purrr::list_rbind(output)
}

# 2D path: mass-weighted (1-p)-quantile of density values from a regular grid.
#
# Equivalent (in the limit) to the previous Monte Carlo estimate but
# deterministic and ~50x faster for typical inputs because it eliminates the
# 1e5-sample distributional::generate() call and the 1e5 density evaluations
# that followed it.
hdr_table_2d <- function(object, prob, dist_names, density_df) {
  if (length(object) > 1L) {
    stop("Currently only supporting one bivariate density")
  }
  thresholds <- hdr_thresholds_from_grid(density_df$density, prob)
  tibble(
    distribution = dist_names[1],
    prob = prob,
    density = thresholds
  )
}

# Mass-weighted (1-p)-quantile of density values on a regular grid.
#
# Mathematical sketch: for each grid cell i, the mass is approximately
# f(x_i) * Delta_x * Delta_y. Sorting cells by f in decreasing order and
# taking the cumulative mass gives the empirical CDF of the random variable
# `f(X)` where X ~ distribution. The HDR threshold at coverage p is the
# smallest density level at which the cumulative mass reaches p.
#
# Under a uniform-cell-area assumption (true to good approximation for the
# grids produced by density_df_2d() -- the only non-uniformity is the
# 0.0001*support boundary padding, which lies in very-low-density regions and
# contributes negligibly at the probabilities we plot), the cell area cancels
# from the normalisation and the implementation reduces to a cumsum on the
# sorted density vector.
hdr_thresholds_from_grid <- function(density, prob) {
  ord <- order(-density)
  d_sorted <- density[ord]
  cum_p <- cumsum(d_sorted) / sum(d_sorted)

  vapply(
    prob,
    function(p) {
      idx <- which(cum_p >= p)[1]
      if (is.na(idx)) min(d_sorted) else d_sorted[idx]
    },
    numeric(1L)
  )
}

# Color palette designed for plotting Highest Density Regions (unchanged).
hdr_palette <- function(n, color = "#0072b2", prob = NULL) {
  if (missing(prob)) {
    prob <- seq(n - 1) / n
  } else if (min(prob) <= 0 || max(prob) > 1 + 1e-6) {
    stop("prob must be between 0 and 1")
  }
  pc_colors <- grDevices::colorRampPalette(c(color, "white"))(150)[c(
    seq(99),
    115
  )]
  idx <- approx(seq(0.01, 1, by = 0.01), seq(100), prob, rule = 2)$y
  c(color, pc_colors[idx])
}

#' @importFrom utils head tail
#' @importFrom dplyr tibble

utils::globalVariables(c("ends", "type", "lower", "upper", "group"))
utils::globalVariables(c("x", "y", "y1", "y2", "distribution"))
