#' Biplot of a two-dimensional projection
#'
#' @description Draw a two-dimensional projection of the scores with the
#' original variable axes overlaid as arrows (loadings), as in a biplot. Pass
#' `object` (the output of [stats::prcomp()] or an `rrcov::Pca*` function);
#' otherwise supply `scores` and `loadings` directly. All scores should be
#' centred about the origin. The arrows are stretched by a common factor so that
#' the longest arrow just reaches the edge of the point cloud, and only loadings
#' longer than `label_threshold` are labelled.
#'
#' @param object Optionally, the output of [stats::prcomp()] or an `rrcov::Pca*`
#' function. If supplied, the scores and loadings are extracted from it and the
#' `scores` and `loadings` arguments are ignored.
#' @param scores A matrix or data frame of scores centred about the origin,
#' with the first two columns used as the horizontal and vertical coordinates.
#' Ignored if `object` is supplied.
#' @param loadings A matrix or data frame of loadings, with row names giving the
#' variable names and the first two columns used as the arrow directions.
#' Ignored if `object` is supplied.
#' @param label_threshold Only loadings whose absolute length exceeds this
#' threshold are labelled. The default of `0` labels every non-zero loading.
#' @param arrow_colour Colour of the arrows and labels.
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#' @return A `ggplot` object.
#' @author Rob J Hyndman
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R",
#' Chapter 9, \url{https://OTexts.com/weird/}.
#' @examples
#' oldfaithful[, c("duration", "waiting")] |>
#'   prcomp(scale = TRUE) |>
#'   biplot_projection()
#' @importFrom ggplot2 geom_point geom_segment geom_text labs expansion scale_x_continuous
#' @importFrom grid unit arrow
#' @export
biplot_projection <- function(
  object = NULL,
  scores = NULL,
  loadings = NULL,
  label_threshold = 0,
  arrow_colour = "#c14b14",
  ...
) {
  if (!is.null(object)) {
    if (inherits(object, "prcomp")) {
      stopifnot(ncol(object$x) >= 2L)
      scores <- object$x[, 1:2]
      loadings <- object$rotation[, 1:2]
    } else if (inherits(object, "Pca")) {
      stopifnot(ncol(object$scores) >= 2L)
      scores <- object$scores[, 1:2]
      loadings <- object$loadings[, 1:2]
    } else {
      stop("object must be the output of prcomp() or an rrcov::Pca* function")
    }
  } else {
    if (is.null(scores) || is.null(loadings)) {
      stop("Supply object, or both scores and loadings.")
    }
    if (ncol(scores) < 2L || ncol(loadings) < 2L) {
      stop("scores and loadings must have at least two columns.")
    }
  }
  scores <- as.data.frame(scores)
  # Check scores contain positive and negative values
  if (
    !(any(scores[, 1] > 0) &&
      any(scores[, 1] < 0) &&
      any(scores[, 2] > 0) &&
      any(scores[, 2] < 0))
  ) {
    warning("scores should be centred about the origin.")
  }
  axis_labels <- colnames(scores)[1:2]
  colnames(scores)[1:2] <- c("x", "y")
  rn_loadings <- row.names(loadings)
  loadings <- as.data.frame(loadings)
  loadings$varname <- rn_loadings
  colnames(loadings)[1:2] <- c("x", "y")
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
  labels <- loadings[loadings$x^2 + loadings$y^2 > label_threshold^2, ]
  # Only expand the x axis on a side that has a label running off the edge:
  # positive-x labels extend right, negative-x labels extend left.
  x_expand <- expansion(
    mult = c(
      if (any(labels$x < 0)) 0.08 else 0.05,
      if (any(labels$x > 0)) 0.08 else 0.05
    )
  )
  ggplot(scores, aes(x = x, y = y)) +
    geom_point(...) +
    geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = arrow_scale * x, yend = arrow_scale * y),
      arrow = arrow(length = unit(1 / 2, "picas"), type = "closed", angle = 15),
      colour = arrow_colour
    ) +
    geom_text(
      data = labels,
      aes(
        label = varname,
        x = arrow_scale * x * 1.02,
        y = arrow_scale * y * 1.02,
        hjust = ifelse(x > 0, 0, 1)
      ),
      colour = arrow_colour
    ) +
    scale_x_continuous(expand = x_expand) +
    labs(x = axis_labels[1], y = axis_labels[2])
}

utils::globalVariables("varname")
