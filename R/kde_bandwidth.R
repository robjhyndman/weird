#' Robust bandwidth estimation for kernel density estimation
#'
#' Bandwidth matrices are estimated using either a robust version of the normal
#' reference rule, or using the approach of Hyndman, Kandanaarachchi & Turner (2026).
#'
#' @param data A numeric matrix or data frame.
#' @param method A character string giving the method to use. Possibilities are:
#' `"normal"` (normal reference rule),
#' `"robust"` (a robust version of the normal reference rule, the default),
#' `"plugin"` (a plugin estimator), and
#' `"lookout"` (the bandwidth matrix estimate of Hyndman, Kandanaarachchi & Turner, 2026).
#' @param ... Additional arguments are ignored unless `method = "lookout"`, when
#' they are passed to [lookout::find_tda_bw()].
#' @references Rob J Hyndman, Sevvandi Kandanaarachchi & Katharine Turner (2026)
#' "When lookout sees crackle: Anomaly detection via kernel density estimation",
#' unpublished. \url{https://robjhyndman.com/publications/lookout2.html}
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 2.7 and 3.9,
#' \url{https://OTexts.com/weird/}.
#' @return A matrix of bandwidths (or a scalar in the case of univariate data).
#' @author Rob J Hyndman
#' @examples
#' # Univariate bandwidth calculation
#' kde_bandwidth(oldfaithful$duration)
#' # Bivariate bandwidth calculation
#' kde_bandwidth(oldfaithful[, c("duration", "waiting")])
#' @export

kde_bandwidth <- function(
  data,
  method = c("robust", "normal", "plugin", "lookout"),
  ...
) {
  method <- match.arg(method)
  d <- NCOL(data)
  n <- NROW(data)
  if (method == "plugin") {
    if (d == 1L) {
      return(stats::bw.SJ(data))
    } else {
      return(ks::Hpi(data))
    }
  }
  if (method == "lookout") {
    cc <- lookout::find_tda_bw(mvscale(data), ...)
  } else {
    cc <- (4 / (n * (d + 2)))^(2 / (d + 4))
  }
  if (d == 1L) {
    cc <- sqrt(cc)
    if (method == "normal") {
      S <- stats::sd(data)
    } else {
      S <- robustbase::s_Qn(data)
    }
  } else {
    if (method == "normal") {
      S <- stats::cov(data)
    } else {
      S <- robustbase::covOGK(data, sigmamu = robustbase::s_Qn)$cov
    }
  }
  return(cc * S)
}
