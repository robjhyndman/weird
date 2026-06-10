#' Biplot of a two-dimensional projection
#'
#' @description Draw a two-dimensional projection of the scores with the
#' original variable axes overlaid as arrows (loadings), as in a biplot. Pass
#' `object` (the output of [stats::prcomp()] or an rrcov `Pca*` function);
#' otherwise supply `scores` and `loadings` directly. The arrows are stretched
#' by a common factor so that the longest arrow just reaches the edge of the
#' point cloud, and only loadings longer than `label_threshold` are labelled.
#'
#' @param object Optionally, the output of [stats::prcomp()] or an rrcov `Pca*`
#' function. If supplied, the scores and loadings are extracted from it and the
#' `scores` and `loadings` arguments are ignored.
#' @param scores A matrix or data frame of scores, with the first two columns
#' used as the horizontal and vertical coordinates. Ignored if `object` is
#' supplied.
#' @param loadings A matrix or data frame of loadings, with row names giving the
#' variable names and the first two columns used as the arrow directions.
#' Ignored if `object` is supplied.
#' @param label_threshold Only loadings whose squared length exceeds this
#' threshold are labelled. The default of `0` labels every loading.
#' @param alpha The transparency of the points and arrows, between `0` (fully
#' transparent) and `1` (fully opaque).
#' @param point_colour Colour of the points.
#' @param arrow_colour Colour of the arrows and labels.
#' @return A `ggplot` object.
#' @author Rob J Hyndman
#' @examples
#' oldfaithful[, c("duration", "waiting")] |>
#'   prcomp(scale = TRUE) |>
#'   biplot_projection()
#' @export
biplot_projection <- function(
  object = NULL,
  scores = NULL,
  loadings = NULL,
  label_threshold = 0,
  alpha = 1,
  point_colour = "#0072B2",
  arrow_colour = "#c14b14",
  ...
) {
  if (!is.null(object)) {
    if (inherits(object, "prcomp")) {
      scores <- object$x[, 1:2]
      loadings <- object$rotation[, 1:2]
    } else if (methods::is(object, "Pca")) {
      scores <- object$scores[, 1:2]
      loadings <- object$loadings[, 1:2]
    } else {
      stop("`object` must be the output of prcomp() or an rrcov Pca* function")
    }
  }
  scores <- as.data.frame(scores)
  axis_labels <- colnames(scores)[1:2]
  colnames(scores)[1:2] <- c("x", "y")
  loadings <- as.data.frame(loadings) |>
    tibble::rownames_to_column("varname")
  colnames(loadings)[2:3] <- c("x", "y")
  # Largest common scale keeping every arrow tip inside the score bounding
  # box: for each arrow take the binding axis, then the tightest arrow wins.
  sx <- ifelse(
    loadings$x > 0,
    max(scores$x) / loadings$x,
    ifelse(loadings$x < 0, min(scores$x) / loadings$x, Inf)
  )
  sy <- ifelse(
    loadings$y > 0,
    max(scores$y) / loadings$y,
    ifelse(loadings$y < 0, min(scores$y) / loadings$y, Inf)
  )
  arrow_scale <- min(pmin(sx, sy))
  ggplot(scores, aes(x = x, y = y)) +
    geom_point(alpha = alpha, colour = point_colour) +
    geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = arrow_scale * x, yend = arrow_scale * y),
      arrow = arrow(length = unit(1 / 2, "picas"), type = "closed", angle = 15),
      colour = arrow_colour,
      alpha = alpha
    ) +
    geom_text(
      data = dplyr::filter(loadings, x^2 + y^2 > label_threshold),
      aes(
        label = varname,
        x = arrow_scale * x,
        y = arrow_scale * y * 1.05
      ),
      colour = arrow_colour,
      ...
    ) +
    labs(x = axis_labels[1], y = axis_labels[2])
}
