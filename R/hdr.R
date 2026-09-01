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
    var1 <- as.symbol(names(data)[1])
    if (NCOL(data) > 1L) {
      message("No variable selected. Using ", as.character(substitute(var1)))
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
  prob <- sort(prob)

  # Set up color palette
  hdr_colors <- make_hdr_colors(dist, color, prob)

  # Pre-compute density grid once for use by both hdr_table() and gg_density*,
  # and density at the data points once for use by both make_threshold() and
  # show_data() (dist is always a single dist_kde distribution here).
  df <- density_df(dist)
  den_at_data <- kde_den_at_data(dist)
  threshold <- make_threshold(dist, prob, df, den_at_data = den_at_data)
  show_x <- show_data(
    dist,
    prob,
    threshold,
    anomalies = show_anomalies,
    den_at_data = den_at_data
  )
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
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 2.7, 3.4. \url{https://OTexts.com/weird/}.
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
  if (d == 1) {
    density_df <- NULL
  } else if (d == 2) {
    density_df <- make_density_df_2d(object)
  } else {
    stop("Not implemented for dimensions greater than 2")
  }
  hdr_table_with_data(object, prob, density_df)
}

hdr_table_with_data <- function(object, prob, density_df, den_at_data = NULL) {
  d <- dimension_dist(object)
  prob <- sort(unique(prob), decreasing = TRUE)
  dist_names <- names_dist(object)

  output <- if (d == 1L) {
    hdr_table_1d(object, prob, dist_names, den_at_data = den_at_data)
  } else {
    hdr_table_2d(
      object,
      prob,
      dist_names,
      density_df = density_df,
      den_at_data = den_at_data
    )
  }

  output |> dplyr::arrange(distribution, prob)
}

# 1D path: compute interval endpoints and threshold density separately for
# each distribution, since dist_kde has a much cheaper path available
# (hdr_intervals_kde()) than the generic distributional::hdr() dispatch.
# `den_at_data`, when supplied, is the density at the data points for
# `object`'s single distribution (see kde_den_at_data()), letting a caller
# that already has it (gg_hdrboxplot()) avoid recomputing it here.
hdr_table_1d <- function(object, prob, dist_names, den_at_data = NULL) {
  reuse <- !is.null(den_at_data) && length(object) == 1L
  per_dist <- mapply(
    function(dist, name) {
      hdr_intervals_1d(dist, prob, den_at_data = if (reuse) den_at_data) |>
        dplyr::mutate(distribution = name, .before = 1)
    },
    dist = as.list(object),
    name = dist_names,
    SIMPLIFY = FALSE
  )
  do.call(rbind, per_dist)
}

# Interval endpoints and threshold density for the HDR of a single 1d
# distribution, for each requested prob. Some distributions (e.g. a
# multimodal dist_kde) can have several disjoint intervals per prob.
hdr_intervals_1d <- function(object, prob, den_at_data = NULL) {
  if ("kde" %in% stats::family(object)) {
    hdr_intervals_kde(object, prob, den_at_data = den_at_data)
  } else {
    # Use distributional::hdr() for canonical interval endpoints. Threshold
    # density is read at each lower endpoint and then averaged to smooth
    # floating-point noise across multiple intervals at the same level.
    do.call(
      rbind,
      lapply(prob, function(p) {
        hdri <- distributional::hdr(object, size = p * 100, n = 4096)
        lower <- vctrs::field(hdri, "lower")[[1]]
        upper <- vctrs::field(hdri, "upper")[[1]]
        density <- mean(unlist(density(object, at = lower)))
        tibble(prob = p, lower = lower, upper = upper, density = density)
      })
    )
  }
}

# Interval endpoints and threshold density for a single dist_kde
# distribution, for each requested prob. Only falpha (the threshold) depends
# on prob, so the density at the data points and the quantile grid used to
# trace interval boundaries are computed once here, rather than once per
# prob inside distributional::hdr()/hdr.dist_kde(). `den_at_data` lets a
# caller that already has the density at the data points (kde_den_at_data())
# pass it in rather than have it recomputed.
hdr_intervals_kde <- function(object, prob, n = 4096, den_at_data = NULL) {
  if (is.null(den_at_data)) {
    den_at_data <- kde_den_at_data(object)
  }
  grid_x <- unlist(quantile(object, seq(0.5 / n, 1 - 0.5 / n, length.out = n)))
  grid_y <- unlist(density(object, at = grid_x))

  do.call(
    rbind,
    lapply(prob, function(p) {
      falpha <- hdr_thresholds_from_data(den_at_data, p)
      hdr <- crossing_alpha(falpha, grid_x, grid_y)
      lower <- sort(hdr[seq_along(hdr) %% 2 == 1])
      upper <- sort(hdr[seq_along(hdr) %% 2 == 0])
      tibble(prob = p, lower = lower, upper = upper, density = falpha)
    })
  )
}

# Density at the data points used to fit a single KDE distribution.
kde_den_at_data <- function(object) {
  x <- vctrs::vec_data(object)[[1]]$kde$x
  unlist(density(object, at = x))
}

# 2D path. For a KDE, falpha is the (1 - p) quantile of the density evaluated
# at the data points, matching the 1D path (hdr.dist_kde). This keeps the HDR
# threshold consistent with surprisal-based anomalies, which are also computed
# from the density at the data points. For non-KDE densities, there is no
# sample to estimate falpha this way, so we fall back to the mass-weighted
# (1-p)-quantile of density values from the regular grid.
hdr_table_2d <- function(
  object,
  prob,
  dist_names,
  density_df,
  den_at_data = NULL
) {
  if (length(object) > 1L) {
    stop("Currently only supporting one bivariate density")
  }
  is_kde <- "kde" %in% stats::family(object)
  if (!is_kde) {
    thresholds <- hdr_thresholds_from_grid(density_df$density, prob)
  } else {
    if (is.null(den_at_data)) {
      den_at_data <- kde_den_at_data(object)
    }
    thresholds <- hdr_thresholds_from_data(den_at_data, prob)
  }
  tibble(
    distribution = dist_names[1],
    prob = prob,
    density = thresholds
  )
}

# falpha is the (1 - p) quantile of the density evaluated at the data points.
hdr_thresholds_from_data <- function(den_at_data, prob) {
  vapply(
    prob,
    function(p) {
      stats::quantile(den_at_data, probs = 1 - p, type = 8, names = FALSE)
    },
    numeric(1L)
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

#' @title Highest density regions for each observation
#' @description
#' For a `dist_kde` object, determine which highest density region (HDR)
#' each observation falls in, for one or more coverage probabilities. Some
#' densities are multimodal, so the HDR for a given `prob` can consist of
#' several disjoint regions; the returned columns identify the specific
#' region (`1`, `2`, ...) that each observation falls in, ordered from
#' lowest to highest along the first variable, with `NA` for observations
#' outside the HDR at that `prob`.
#' @param object A `dist_kde` object, as returned by `dist_kde()`, containing
#' a single distribution estimated from univariate or bivariate data.
#' @param prob A numeric vector of probabilities giving the HDR coverage
#' (between 0 and 1).
#' @return A tibble containing the data used to estimate `object`,
#' along with one additional integer
#' column per element of `prob` (named `hdr_<100*prob>`) showing which region
#' of the corresponding HDR each observation falls in.
#' @author Rob J Hyndman
#' @seealso \code{\link{hdr_table}}, \code{\link{gg_hdrboxplot}}
#' @examples
#' dist_kde(oldfaithful$duration) |> hdr_regions(c(0.5, 0.95))
#' dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_regions(0.90)
#' @export
hdr_regions <- function(object, prob) {
  if (!("kde" %in% stats::family(object))) {
    stop("object must be a dist_kde object")
  }
  if (length(object) != 1L) {
    stop("hdr_regions() requires a single dist_kde distribution")
  }
  d <- dimension_dist(object)
  prob <- sort(unique(prob))
  if (d == 1L) {
    hdr_regions_1d(object, prob)
  } else if (d == 2L) {
    hdr_regions_2d(object, prob)
  } else {
    stop("hdr_regions() is not implemented for dimensions greater than 2")
  }
}

hdr_colname <- function(prob) {
  paste0("hdr_", 100 * prob)
}

hdr_regions_1d <- function(object, prob) {
  kde <- vctrs::vec_data(object)[[1]]$kde
  x <- as.vector(kde$x)
  xname <- colnames(kde$x)
  xname <- if (is.null(xname)) "x" else xname[1]
  intervals <- hdr_intervals_kde(object, prob)

  out <- tibble(x = x)
  names(out)[1] <- xname
  for (p in prob) {
    ivl <- intervals[intervals$prob == p, ]
    region <- rep(NA_integer_, length(x))
    for (j in seq_len(nrow(ivl))) {
      region[x >= ivl$lower[j] & x <= ivl$upper[j]] <- j
    }
    out[[hdr_colname(p)]] <- region
  }
  out
}

hdr_regions_2d <- function(object, prob) {
  kde <- vctrs::vec_data(object)[[1]]$kde
  xy <- kde$x
  varnames <- colnames(xy)
  if (is.null(varnames)) {
    varnames <- c("x", "y")
  }
  ex <- kde$eval.points[[1]]
  ey <- kde$eval.points[[2]]
  grid_xy <- as.matrix(expand.grid(x = ex, y = ey))
  den_data <- kde_den_at_data(object)
  nn_idx <- RANN::nn2(data = grid_xy, query = xy, k = 1L)$nn.idx[, 1]
  thresholds <- hdr_thresholds_from_data(den_data, prob)

  out <- tibble(x = xy[, 1], y = xy[, 2])
  names(out)[1:2] <- varnames
  for (i in seq_along(prob)) {
    threshold <- thresholds[i]
    # kde$estimate is already an nx x ny matrix, so this comparison keeps its shape.
    labels <- label_components(kde$estimate > threshold)
    region <- as.vector(labels)[nn_idx]
    region[region == 0L | den_data <= threshold] <- NA_integer_
    out[[hdr_colname(prob[i])]] <- region
  }
  out
}

# Label the 4-connected components of a logical matrix, returning an integer
# matrix of the same shape (0 = FALSE cells). Grid cells one row/column apart
# are at Euclidean distance 1 and diagonal neighbours at distance sqrt(2), so
# dbscan() with eps = 1.1 and minPts = 1 links only 4-connected cells and
# leaves no cell unclustered.
label_components <- function(mask) {
  idx <- which(mask, arr.ind = TRUE)
  labels <- matrix(0L, nrow(mask), ncol(mask))
  if (nrow(idx) == 0L) {
    return(labels)
  }
  labels[idx] <- dbscan::dbscan(idx, eps = 1.1, minPts = 1)$cluster
  labels
}

# Color palette for plotting Highest Density Regions.
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

make_hdr_colors <- function(object, colors, prob) {
  hdr_colors <- lapply(colors, function(u) {
    hdr_palette(color = u, prob = c(prob, 1))
  })
  names(hdr_colors) <- names_dist(object, unique = TRUE)
  hdr_colors
}

make_threshold <- function(dist, prob, df, den_at_data = NULL) {
  hdr_table_with_data(dist, prob, density_df = df, den_at_data = den_at_data) |>
    dplyr::transmute(
      level = 100 * prob,
      distribution = distribution,
      threshold = density
    ) |>
    dplyr::distinct()
}

#' @importFrom utils head tail
#' @importFrom dplyr tibble

utils::globalVariables(c("ends", "type", "lower", "upper", "group"))
utils::globalVariables(c("x", "y", "y1", "y2", "distribution"))
