#' @title Local outlier factors
#' @description Compute local outlier factors using k nearest neighbours. A local
#' outlier factor is a measure of how anomalous each observation is based on
#' the density of neighbouring points.
#' The function uses \code{dbscan::\link[dbscan]{lof}} to do the calculation.
#' @param y Numerical matrix or vector of data
#' @param k Number of neighbours to include, not counting the observation itself. Default: 10.
#' @param ... Additional arguments passed to \code{dbscan::\link[dbscan]{lof}}
#' @return Numerical vector containing LOF values. An observation has an infinite
#' LOF when its neighbourhood includes at least \code{k + 1} identical observations
#' (whose local reachability density is infinite) but it is not one of them;
#' the identical observations themselves have LOF values of 1.
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 6.6,
#' \url{https://OTexts.com/weird/}.
#' @author Rob J Hyndman
#' @examples
#' y <- c(rnorm(49), 5)
#' lof_scores(y)
#' @export
#' @seealso
#'  \code{dbscan::\link[dbscan]{lof}}
#' @importFrom dbscan lof
lof_scores <- function(y, k = 10, ...) {
  y <- na.omit(y)
  lof <- dbscan::lof(as.matrix(y), minPts = k + 1, ...)
  return(lof)
}

#' @title GLOSH scores
#' @description Compute Global-Local Outlier Score from Hierarchies. This is based
#' on hierarchical clustering, using core distances to the k-th nearest neighbour. The resulting
#' outlier score is a measure of how anomalous each observation is.
#' The function uses \code{dbscan::\link[dbscan]{hdbscan}} to do the calculation.
#' @param y Numerical matrix or vector of data
#' @param k Number of neighbours to include, not counting the observation itself. Default: 10.
#' @param ... Additional arguments passed to \code{dbscan::\link[dbscan]{hdbscan}}
#' @return Numerical vector containing GLOSH values
#' @author Rob J Hyndman
#' @examples
#' y <- c(rnorm(49), 5)
#' glosh_scores(y)
#' @export
#' @seealso
#'  \code{dbscan::\link[dbscan]{glosh}}
#' @importFrom dbscan hdbscan
glosh_scores <- function(y, k = 10, ...) {
  dbscan::hdbscan(as.matrix(y), minPts = k + 1, ...)$outlier_scores
}
