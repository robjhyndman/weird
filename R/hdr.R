#' @title HDR plot
#' @description Produces a 1d or 2d box plot of HDR regions. The darker regions
#' contain observations with higher probability, while the lighter regions contain
#' points with lower probability. Points outside the largest HDR are shown as
#' individual points. Points with lookout probabilities
#' less than 0.05 are optionally shown in red.
#' @details The original HDR boxplot proposed by Hyndman (1996), can be produced with
#' all arguments set to their defaults other than `show_anomalies`.
#' @param data A data frame or matrix containing the data.
#' @param var1 The name of the first variable to plot (a bare expression).
#' @param var2 Optionally, the name of the second variable to plot (a bare expression).
#' @param prob A numeric vector specifying the coverage probabilities for the HDRs.
#' @param show_points A logical argument indicating if a regular HDR plot is required
#' (\code{FALSE}), or whether to show the individual observations in the same colors (\code{TRUE}).
#' @param color The base color to use for the mode. Colors for the HDRs are generated
#' by whitening this color.
#' @param scatterplot Equivalent to `show_points`. Included for compatability
#' with \code{\link{gg_bagplot}()}.
#' @param ngrid Number of grid points to use for the density function.
#' @param ... Other arguments passed to \code{\link[ks]{kde}}.
#' @return A ggplot object showing an HDR plot or scatterplot of the data.
#' @author Rob J Hyndman
#' @references Hyndman, R J (1996) Computing and Graphing Highest Density Regions,
#' *The American Statistician*, **50**(2), 120–126. \url{https://robjhyndman.com/publications/hdr/}
#' Kandanaarachchi, S & Hyndman, R J (2022) "Leave-one-out kernel density estimates for outlier detection",
#' *J Computational & Graphical Statistics*, **31**(2), 586-599. \url{https://robjhyndman.com/publications/lookout/}
#' @examples
#' df <- data.frame(x = c(rnorm(1000), rnorm(1000, 5, 1)))
#' gg_hdrboxplot(df, x)
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   gg_hdrboxplot(Average)
#' oldfaithful |>
#'   filter(duration < 7000, waiting < 7000) |>
#'   gg_hdrboxplot(duration, waiting, show_points = TRUE)
#'
#' @rdname hdrplot
#' @export

gg_hdrboxplot <- function(data, var1, var2 = NULL, prob = c(0.5, 0.99),
                          color = "#0072b2",
                          show_points = FALSE,
                          scatterplot = FALSE,
                          ngrid = 501, ...) {
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
  dist <- dist_kde(data[, seq(d)], multiplier = 2)
  hdr <- dplyr::if_else(show_points | scatterplot, "points", "fill")

  # Set up color palette
  prob <- sort(prob)
  hdr_colors <- list(hdr_palette(color = color, prob = c(prob, 0.995)))
  names(hdr_colors) <- names_dist(dist)

  # HDR thresholds
  threshold <- hdr_table(dist, prob) |>
    dplyr::transmute(level = 100 * prob, Distribution = distribution, threshold = density) |>
    dplyr::distinct()

  # Data to plot
  show_x <- show_data(dist, prob, threshold, anomalies = TRUE)

  # Call gg_density functions
  if (d == 2L) {
    gg_density2(dist,
      df = make_density_df(dist, ngrid = ngrid),
      show_x = show_x,
      threshold = threshold,
      prob = prob,
      hdr = hdr,
      hdr_colors = hdr_colors,
      alpha = NULL,
      show_points = TRUE,
      show_mode = TRUE,
      show_anomalies = TRUE
    ) +
      ggplot2::guides(fill = "none", color = "none")
  } else {
    gg_density1(dist,
      df = make_density_df(dist, ngrid = ngrid),
      show_x = show_x,
      threshold = threshold,
      prob = prob,
      hdr = hdr,
      hdr_colors = hdr_colors,
      alpha = NULL,
      show_density = FALSE,
      jitter = TRUE,
      show_points = TRUE,
      show_mode = TRUE,
      show_anomalies = TRUE
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
#' @param density_only If `TRUE`, only the density values are returned for each
#' distribution, not the end points of the interval. (Default `FALSE` for
#' univariate distributions, and `TRUE` for multivariate distributions.)
#' @return A tibble
#' @author Rob J Hyndman
#' @examples
#' # Univariate HDRs
#' c(dist_normal(), dist_kde(c(rnorm(100), rnorm(100, 3, 1)))) |>
#'   hdr_table(c(0.5, 0.95))
#' dist_kde(oldfaithful$duration) |> hdr_table(0.95)
#' # Bivariate HDRs
#' dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_table(0.90)
#' @export
hdr_table <- function(object, prob, density_only = FALSE) {
  d <- dimension_dist(object)
  prob <- sort(unique(prob), decreasing = TRUE)
  dist_names <- names_dist(object)
  if (d == 1L) {
    output <- lapply(
      prob,
      function(p) {
        hdri <- distributional::hdr(object, size = p * 100, n = 1024)
        # Extract limits
        hdri <- tibble(
          prob = p,
          distribution = dist_names,
          lower = vctrs::field(hdri, "lower"),
          upper = vctrs::field(hdri, "upper")
        ) |>
          tidyr::unnest(c(lower, upper))
        mapply(
          function(dist, hdr) {
            hdr |>
              dplyr::mutate(density = unlist(density(dist, at = lower)))
          },
          dist = as.list(object), hdr = split(hdri, hdri$distribution)[dist_names],
          SIMPLIFY = FALSE
        ) |>
          purrr::list_rbind()
      }
    )
    # For multiple intervals, average the density values at the ends
    # to avoid having different values
    output <- lapply(output, function(df) {
      df$density <- mean(df$density)
      return(df)
    })
  } else {
    output <- mapply(
      function(u, dist) {
        r <- distributional::generate(u, times = 1e5)[[1]]
        fi <- density(u, at = as.matrix(r))[[1]]
        tibble(
          distribution = dist,
          prob = prob,
          density = quantile(fi, prob = 1 - prob, type = 8)
        )
      },
      u = as.list(object), dist = as.list(names_dist(object)),
      SIMPLIFY = FALSE
    )
  }
  purrr::list_rbind(output) |>
    dplyr::arrange(distribution, prob)
}

# Color palette designed for plotting Highest Density Regions
#
# A sequential color palette is returned, with the first color being `color`,
# and the rest of the colors being a mix of `color` with increasing amounts of white.
# If `prob` is provided, then the mixing proportions are determined by `prob` (and
# n is ignored). Otherwise the mixing proportions are equally spaced between 0 and 1.
#
# @param n Number of colors in palette.
# @param color First color of vector.
# @param prob Vector of probabilities between 0 and 1.
# @return A function that returns a vector of colors of length `length(prob) + 1`.
# @examples
# hdr_palette(prob = c(0.5, 0.99))

hdr_palette <- function(n, color = "#0072b2", prob = NULL) {
  if (missing(prob)) {
    prob <- seq(n - 1) / n
  } else if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  pc_colors <- grDevices::colorRampPalette(c(color, "white"))(180)[32:130]
  idx <- approx(seq(99) / 100, seq(99), prob, rule = 2)$y
  c(color, pc_colors[idx])
}

#' @importFrom utils head tail
#' @importFrom tibble tibble

utils::globalVariables(c("ends", "type", "lower", "upper", "group"))
utils::globalVariables(c("x", "y", "y1", "y2", "distribution"))
