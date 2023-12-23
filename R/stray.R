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
#' threshold <- stray::find_threshold(scores, alpha = 0.01, outtail = "max", p = 0.5, tn = 50)
#' which(scores > threshold)
#' @export
#' @rdname stray_scores
stray_scores <- function(y,  ...) {
  stray::find_HDoutliers(data = y, ...)$out_scores
}

#' @title Stray anomalies
#' @description Test if observations are anomalies according to the stray algorithm.
#' @param y A vector, matrix, or data frame consisting of numerical variables.
#' @param ... Other arguments are passed to \code{\link[stray]{find_HDoutliers}}.
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' y <- c(6, rnorm(49))
#' stray_anomalies(y)
#' # Bivariate data
#' y <- cbind(rnorm(50), c(5, rnorm(49)))
#' stray_anomalies(y)
#' @return Numerical vector containing logical values indicating if the
#' observation is identified as an anomaly using the stray algorithm.
#' @export
stray_anomalies <- function(y,  ...) {
  stray::find_HDoutliers(data = y, ...)$type == "outlier"
}
