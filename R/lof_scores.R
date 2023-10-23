#' @title Local outlier factors
#' @description Compute local outlier factors using k nearest neighbours. A local
#' outlier factor is a measure of how anomalous each observation is based on
#' the density of neighbouring points.
#' The function uses \code{dbscan::\link[dbscan]{lof}} to do the calculation.
#' @param y Numerical matrix or vector of data
#' @param k Number of neighbours to include. Default: 5.
#' @param ... Additional arguments passed to \code{dbscan::\link[dbscan]{lof}}
#' @return Numerical vector containing LOF values
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
  lof <- dbscan::lof(as.matrix(y), minPts = k, ...)
  lof[lof == Inf] <- 0
  return(lof)
}

#' @title GLOSH scores
#' @description Compute Global-Local Outlier Score from Hierarchies. This is based
#' on hierarchical clustering where the minimum cluster size is k. The resulting
#' outlier score is a measure of how anomalous each observation is.
#' The function uses \code{dbscan::\link[dbscan]{hdbscan}} to do the calculation.
#' @param y Numerical matrix or vector of data
#' @param k Minimum cluster size. Default: 5.
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
  dbscan::hdbscan(as.matrix(y), minPts = k, ...)$outlier_scores
}
