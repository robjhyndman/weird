#' @title Kernel density estimate scores and lookout probabilities
#' @description Compute kernel density estimate scores,
#'  defined as minus the log of the kernel density estimate at each observation.
#'  A Gaussian kernel is used, with default h given by a normal reference rule.
#'  Leave-one-out scores are optionally computed.
#'  Lookout probabilities are returned by \code{lookout_prob} computed using a
#'  Generalized Pareto Distribution estimated from the kde scores and applied
#'  to the leave-one-out kde scores. These give the probability of each observation
#'  being an anomaly.
#' @param y Numerical vector of data
#' @param h Bandwidth. Default: \code{\link[stats]{bw.nrd}(y)}
#' @param kernel A character string giving the kernel to be used.
#' @param loo Should leave-one-scores be returned? Default: FALSE
#' @return Numerical vector containing kde scores
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
kde_scores <- function(y, h = stats::bw.nrd(y),
                       kernel = c("gaussian", "epanechnikov"), loo = FALSE) {
  kernel <- match.arg(kernel)
  y <- na.omit(y)
  n <- length(y)
  fi <- approx(stats::density(y, bw = h, kernel = kernel, n = 1e5), xout = y)$y
  if (loo) {
    scores <- -log(pmax(0, (n * fi - K0(kernel, h)) / (n - 1)))
  } else {
    scores <- -log(pmax(0, fi))
  }
  return(scores)
}

#' @rdname kde_scores
#' @export

lookout_prob <- function(y, h = stats::bw.nrd(y),
                         kernel = c("gaussian", "epanechnikov")) {
  kernel <- match.arg(kernel)
  y <- na.omit(y)
  n <- length(y)
  scores <- kde_scores(y, h, kernel)
  loo_scores <- -log(pmax(0, (n * exp(-scores) - K0(kernel, h)) / (n - 1)))
  threshold <- quantile(scores, prob = 0.90, type = 8)
  gpd <- evd::fpot(scores, threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE)
}

K0 <- function(kernel, h) {
  if (kernel == "gaussian") {
    return(dnorm(0, sd = h))
  } else if (kernel == "epanechnikov") {
    return(0.75 / h / sqrt(5))
  } else {
    stop("Unknown kernel")
  }
}
