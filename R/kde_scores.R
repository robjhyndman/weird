#' @title Kernel density estimate scores and lookout probabilities
#' @description Compute kernel density estimate scores,
#'  defined as minus the log of the kernel density estimate at each observation.
#'  A Gaussian kernel is used, with default bandwidth given by a normal reference rule.
#'  Leave-one-out scores are optionally computed.
#'  Lookout probabilities are returned by \code{lookout_prob} computed using a
#'  Generalized Pareto Distribution estimated from the kde scores and applied
#'  to the leave-one-out kde scores. These give the probability of each observation
#'  being an anomaly.
#' @param y Numerical vector or matrix of data (up to 6 dimensions).
#' @param h Bandwidth for univariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param H Bandwidth for multivariate kernel density estimate. Default is \code{\link{kde_bandwidth}}.
#' @param loo Should leave-one-scores be returned? Default: FALSE.
#' @param ... Other arguments are passed to \code{\link[ks]{kde}}.
#' @return Numerical vector containing kde scores.
#' @author Rob J Hyndman
#' @examples
#' # Univariate data
#' y <- c(5, rnorm(49))
#' kde_scores(y)
#' lookout_prob(y)
#' # Bivariate data
#' y <- cbind(rnorm(50), c(5, rnorm(49)))
#' kde_scores(y)
#' lookout_prob(y)
#' @export
#' @rdname kde_scores
#' @seealso
#'  \code{\link{kde_bandwidth}}
#'  \code{\link[ks]{kde}}
#' @importFrom stats quantile density dnorm approx na.omit
#' @importFrom evd fpot pgpd
kde_scores <- function(y, loo = FALSE, h = kde_bandwidth(y), H = kde_bandwidth(y), ...) {
  tmp <- calc_kde_scores(y, h, H, ...)
  if (loo) {
    return(tmp$loo_scores)
  } else {
    return(tmp$scores)
  }
}

#' @rdname kde_scores
#' @export

lookout_prob <- function(y, loo = FALSE, h = kde_bandwidth(y), H = kde_bandwidth(y), ...) {
  tmp <- calc_kde_scores(y, h, H, ...)
  loo_scores <- tmp$loo_scores
  threshold <- stats::quantile(tmp$scores, prob = 0.90, type = 8)
  gpd <- evd::fpot(tmp$scores, threshold = threshold, std.err = FALSE)$estimate
  evd::pgpd(loo_scores,
    loc = threshold,
    scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE
  )
}

# Compute value of density at each observation using kde
calc_kde_scores <- function(y, h = kde_bandwidth(y), H = kde_bandwidth(y), ...) {
  n <- NROW(y)
  d <- NCOL(y)
  # Estimate density at each observation
  if(d == 1L) {
    gridsize = 10001
    K0 = 1/(h * sqrt(2 * pi))
    fi <- ks::kde(y, h = h, gridsize = gridsize, binned = n > 2000,
            eval.points = y, compute.cont = FALSE, ...)$estimate
  } else {
    gridsize = 101
    K0 = det(H)^(-1/2) * (2*pi)^(-d/2)
    fi <- ks::kde(y, H = H, gridsize = gridsize, binned = n > 2000,
            eval.points = y, compute.cont = FALSE, ...)$estimate
  }
  loo_scores <- -log(pmax(0, (n * fi - K0) / (n - 1)))
  scores <- -log(pmax(0, fi))
  return(list(scores = scores, loo_scores = loo_scores))
}
