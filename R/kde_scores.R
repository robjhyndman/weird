#' @title Kernel density estimate scores and lookout probabilities
#' @description Compute kernel density estimate scores,
#'  defined as minus the log of the kernel density estimate at each observation.
#'  A Gaussian kernel is used, with default h given by a normal reference rule.
#'  Leave-one-out scores are optionally computed.
#'  Lookout probabilities are returned by \code{lookout_prob} computed using a
#'  Generalized Pareto Distribution estimated from the kde scores and applied
#'  to the leave-one-out kde scores. These give the probability of each observation
#'  being an anomaly.
#' @param y Numerical vector or matrix of data (up to 6 dimensions).
#' @param loo Should leave-one-scores be returned? Default: FALSE.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @return Numerical vector containing kde scores.
#' @author Rob J Hyndman
#' @examples
#' y <- c(rnorm(49), 5)
#' kde_scores(y)
#' lookout_prob(y)
#' @export
#' @rdname kde_scores
#' @seealso
#'  \code{\link[stats]{bandwidth}}
#'  \code{\link[ks]{kde}}
#' @importFrom stats bw.nrd quantile density dnorm approx na.omit
#' @importFrom evd fpot pgpd
kde_scores <- function(y, loo = FALSE, ...) {
  tmp <- calc_kde_scores(y, ...)
  if (loo) {
    return(tmp$loo_scores)
  } else {
    return(tmp$scores)
  }
}

#' @rdname kde_scores
#' @export

lookout_prob <- function(y, ...) {
  tmp <- calc_kde_scores(y, ...)
  loo_scores <- tmp$loo_scores
  threshold <- stats::quantile(tmp$scores, prob = 0.90, type = 8)
  gpd <- evd::fpot(tmp$scores, threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
}

calc_kde_scores <- function(y, ...) {
  if (NCOL(y) > 1) {
    stop("Not yet implemented for multivariate data.")
  }
  y <- na.omit(y)
  n <- length(y)
  fy <- ks::kde(y, eval.points = y, binned = n > 1000, ...)
  fi <- fy$estimate
  h <- fy$h
  loo_scores <- -log(pmax(0, (n * fi - dnorm(0, 0, h)) / (n - 1)))
  scores <- -log(pmax(0, fi))
  return(list(scores = scores, loo_scores = loo_scores, h = h))
}
