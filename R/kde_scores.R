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
  y <- na.omit(y)
  n <- length(y)
  fy <- ks::kde(y, eval.points = y, ...)
  fi <- fy$estimate
  if (loo) {
    scores <- -log(pmax(0, (n * fi - dnorm(0, y$h)) / (n - 1)))
  } else {
    scores <- -log(pmax(0, fi))
  }
  return(scores)
}

#' @rdname kde_scores
#' @export

lookout_prob <- function(y, ...) {
  y <- na.omit(y)
  n <- length(y)
  scores <- kde_scores(y, ...)
  loo_scores <- -log(pmax(0, (n * exp(-scores) - dnorm(0,h)) / (n - 1)))
  threshold <- quantile(scores, prob = 0.90, type = 8)
  gpd <- evd::fpot(scores, threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE)
}
