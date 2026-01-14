#' Robust bandwidth estimation for kernel density estimation
#'
#' By default, bandwidths are chosen using a robust version of the normal reference
#' rule. Alternatively, they can be estimated using the approach of
#' Hyndman, Kandanaarachchi & Turner (2026) if `lookout = TRUE`. The resulting
#' bandwidth is scaled by `multiplier` (set to 1 by default).
#'
#' @param data A numeric matrix or data frame.
#' @param lookout A logical variable (set to `FALSE`` by default) indicating
#' that the bandwidth matrix estimate of Hyndman, Kandanaarachchi & Turner (2026)
#' is returned.
#' @param multiplier Bandwidth scaling factor (squared if the data dimension is
#' greater than 1).
#' @param ... Additional arguments are ignored if `lookout = FALSE` and passed to
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

kde_bandwidth <- function(data, lookout = FALSE, multiplier = 1, ...) {
  d <- NCOL(data)
  n <- NROW(data)
  if (lookout) {
    cc <- lookout::find_tda_bw(mvscale(data), ...)
  } else {
    cc <- (4 / (n * (d + 2)))^(2 / (d + 4))
  }
  if (d == 1L) {
    cc <- sqrt(cc)
    S <- robustbase::s_Qn(data)
  } else {
    multipler <- multiplier^2
    S <- robustbase::covOGK(data, sigmamu = robustbase::s_Qn)$cov
  }
  return(multiplier * cc * S)
}
