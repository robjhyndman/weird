#' Robust bandwidth estimation for kernel density estimation
#'
#' Bandwidth matrices are estimated using either a robust version of the normal
#' reference rule, or using the approach of Hyndman, Kandanaarachchi & Turner (2026).
#'
#' @param data A numeric matrix or data frame.
#' @param method A character string giving the method to use. If equal to `rnrr`,
#' a robust version of the normal reference rule is used. If equal to `lookout`,
#' the bandwidth matrix estimate of Hyndman, Kandanaarachchi & Turner (2026)
#' is returned.
#' @param ... Additional arguments are ignored if `method = "lookout"` and passed to
#' [lookout::find_tda_bw()] otherwise.
#' @references Rob J Hyndman, Sevvandi Kandanaarachchi & Katharine Turner (2026)
#' "When lookout sees crackle: Anomaly detection via kernel density estimation",
#' unpublished. \url{https://robjhyndman.com/publications/lookout2/}
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R",
#' \url{https://OTexts.com/weird/}
#' @return A matrix of bandwidths (or a scalar in the case of univariate data).
#' @author Rob J Hyndman
#' @examples
#' # Univariate bandwidth calculation
#' kde_bandwidth(oldfaithful$duration)
#' # Bivariate bandwidth calculation
#' kde_bandwidth(oldfaithful[, c("duration", "waiting")])
#' @export

kde_bandwidth <- function(data, method = c("rnrr","lookout"), ...) {
  method <- match.arg(method)
  d <- NCOL(data)
  n <- NROW(data)
  if (method == "lookout") {
    cc <- lookout::find_tda_bw(mvscale(data), ...)
  } else {
    cc <- (4 / (n * (d + 2)))^(2 / (d + 4))
  }
  if (d == 1L) {
    cc <- sqrt(cc)
    S <- robustbase::s_Qn(data)
  } else {
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_Qn)$cov
  }
  return(cc * S)
}
