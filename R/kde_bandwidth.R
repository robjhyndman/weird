#' Robust bandwidth estimation for kernel density estimation
#'
#' @param data A numeric matrix or data frame.
#' @param multiplier Bandwidths are chosen using a robust version of the
#' normal reference rule multiplied by a constant. The default is 1.
#' @return A matrix of bandwidths (or scalar in the case of univariate data).
#' @author Rob J Hyndman
#' @examples
#' # Univariate bandwidth calculation
#' kde_bandwidth(oldfaithful$duration)
#' # Bivariate bandwidth calculation
#' kde_bandwidth(oldfaithful[, c("duration", "waiting")])
#' @export

kde_bandwidth <- function(data, multiplier = 1) {
  d <- NCOL(data)
  n <- NROW(data)
  if (d == 1L) {
    return(multiplier * 1.059224 * robustbase::s_Qn(data) * n^(-0.2))
  } else {
    # Find robust covariance matrix of data
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_Qn)$cov
    return(multiplier^2 * (4 / (n * (d + 2)))^(2 / (d + 4)) * S)
  }
}
