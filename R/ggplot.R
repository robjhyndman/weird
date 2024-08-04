#' Produce ggplot of densities from kde objects in 1 or 2 dimensions
#'
#' @details
#' This function produces a ggplot of the density estimate produced by `ks::kde()`.
#' For univariate densities, it produces a line plot of the density function, with
#' an optional ribbon showing some highest density regions (HDRs) and/or the observations.
#' For bivariate densities, it produces a contour plot of the density function, with
#' the observations optionally shown as points.
#' The mode can also be drawn as a point with the HDRs.
#' For bivariate densities, the combination of `fill = TRUE`, `show_points = TRUE`,
#' `show_mode = TRUE`, and `prob = c(0.5, 0.99)` is equivalent to an HDR boxplot.
#' For univariate densities,  the combination of `show_hdr = TRUE`, `show_points = TRUE`,
#' `show_mode = TRUE`, and `prob = c(0.5, 0.99)` is equivalent to an HDR boxplot.
#'
#' @param object Probability density function as estimated by `ks::kde()`.
#' @param prob Probability of the HDR contours to be drawn (for a bivariate plot only).
#' @param fill If `TRUE`, and the density is bivariate, the bivariate contours
#' are shown as filled regions rather than lines.
#' @param show_hdr If `TRUE`, and the density is univariate, then the HDR regions
#' specified by `prob` are shown as a ribbon below the density.
#' @param show_points If `TRUE`, then individual points are plotted.
#' @param show_mode If `TRUE`, then the mode of the distribution is shown.
#' @param show_lookout If `TRUE`, then the observations with lookout probabilities less than 0.05 are shown in red.
#' @param color Color used for mode and HDR contours. If `fill`, this is the
#' base color used in constructing the palette.
#' @param alpha Transparency of points. When `fill` is `FALSE`, defaults to
#' min(1, 1000/n), where n is the number of observations. Otherwise, set to 1.
#' @param ... Additional arguments are currently ignored.
#' @return A ggplot object.
#' @author Rob J Hyndman
#' @examples
#' # Univariate density
#' c(rnorm(500), rnorm(500, 4, 1.5)) |>
#'   ks::kde() |>
#'   autoplot(show_hdr = TRUE, prob = c(0.5, 0.95), color = "#c14b14")
#' ymat <- tibble(y1 = rnorm(5000), y2 = y1 + rnorm(5000))
#' ymat |>
#'   ks::kde(H = kde_bandwidth(ymat)) |>
#'   autoplot(show_points = TRUE, alpha = 0.1, fill = TRUE)
#' @export

autoplot.kde <- function(
    object, prob = seq(9) / 10, fill = FALSE,
    show_hdr = FALSE, show_points = FALSE, show_mode = FALSE, show_lookout = FALSE,
    color = "#00659e", alpha = ifelse(fill, 1, min(1, 1000 / NROW(object$x))),
    ...) {
  dist <- distributional::new_dist(kde = list(object), class = "dist_kde")
  gg_density(dist,
    prob = prob, fill = fill, show_hdr = show_hdr, show_points = show_points,
    show_mode = show_mode, show_lookout = show_lookout, color = color,
    alpha = alpha, ...
  )
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

hdr_palette <- function(n, color = "#00659e", prob = NULL) {
  if (missing(prob)) {
    prob <- seq(n - 1) / n
  } else if (min(prob) <= 0 | max(prob) >= 1) {
    stop("prob must be between 0 and 1")
  }
  pc_colors <- grDevices::colorRampPalette(c(color, "white"))(150)[2:100]
  idx <- approx(seq(99) / 100, seq(99), prob, rule = 2)$y
  c(color, pc_colors[idx])
}

utils::globalVariables(c("x", "y", "y1", "y2"))
