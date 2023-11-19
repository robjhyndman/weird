#' @title Stray scores
#' @description Compute stray scores indicating how anomalous each observation is.
#' @param y A vector, matrix, or data frame consisting of numerical variables.
#' @param ... Other arguments are passed to \code{\link[stray]{find_HDoutliers}}.
#' @return Numerical vector containing stray scores.
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' y <- c(6, rnorm(49))
#' scores <- stray_scores(y)
#' threshold <- stray::find_threshold(scores)
#' which(scores > threshold)
#' stray_anomalies(y)
#' # Bivariate data
#' y <- cbind(rnorm(50), c(5, rnorm(49)))
#' stray_anomalies(y)
#' @export
#' @rdname stray_scores
stray_scores <- function(y,  ...) {
  stray::find_HDoutliers(data = y, ...)$out_scores
}

#' @rdname stray_scores
#' @return Numerical vector containing logical values indicating if the
#' observation is identified as an anomaly using the stray algorithm.
#' @export
stray_anomalies <- function(y,  ...) {
  stray::find_HDoutliers(data = y, ...)$type == "outlier"
}
