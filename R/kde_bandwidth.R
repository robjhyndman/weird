#' Robust bandwidth estimation for kernel density estimation
#'
#' @param data A numeric matrix or data frame.
#' @param method Method to use for selecting the bandwidth.
#' `robust_normal` uses a robust version of the normal reference rule.
#' `lookout` uses the topological data analysis approach that is part of the lookout algorithm.
#' @param max.iter How many times should the `lookout` method be iterated. That is, outliers
#' (probability < 0.05) are removed and the bandwidth is re-computed from the
#' remaining observations.
#' @return A matrix of bandwidths (or scalar in the case of univariate data).
#' @author Rob J Hyndman
#' @examples
#' # Univariate bandwidth calculation
#' kde_bandwidth(oldfaithful$duration)
#' # Bivariate bandwidth calculation
#' kde_bandwidth(oldfaithful[, 2:3])
#' @export

kde_bandwidth <- function(data, method = c("robust_normal", "double", "lookout"),
                          max.iter = 2) {
  method <- match.arg(method)
  d <- NCOL(data)
  n <- NROW(data)
  if (d > 1) {
    # Find robust covariance matrix of data
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_IQR)$cov
  }
  if (method != "lookout") {
    k <- ifelse(method == "double", 2, 1)
    if (d == 1L) {
      return(k * 1.06 * robustbase::s_IQR(data) * n^(-0.2))
    } else {
      return((4 / (n * (d + 2)))^(2 / (d + 4)) * k^2 * S)
    }
  } else {
    stop("Not yet implemented")
    # # Initial estimate
    # if(d == 1L) {
    #   S <- 1
    # } else {
    #   # Normalize data
    #   U <- chol(solve(S))
    #   data <- as.matrix(data) %*% t(U)
    # }
    # h <- lookout::find_tda_bw(data, fast = (n > 1000)) |>
    #   suppressWarnings()
    # iter <- 1
    # oldh <- 0
    # while(iter < max.iter & h != oldh) {
    #   iter <- iter + 1
    #   oldh <- h
    #   scores <- calc_kde_scores(data, h=h, H=h*diag(d))
    #   p <- lookout(density_scores = scores$scores, loo_scores = scores$loo) |>
    #     suppressWarnings()
    #   data <- as.matrix(data)[p > 0.05,]
    #   # Refined estimate
    #   h <- lookout::find_tda_bw(data, fast = (n > 1000))
    # }
    # return(h * S)
  }
}
