#' @title Kernel density estimate scores and lookout probabilities
#' @description Compute kernel density estimate scores,
#'  defined as minus the log of the kernel density estimate at each observation.
#'  A Gaussian kernel is used, with default h given by a normal reference rule.
#'  Leave-one-out scores are optionally computed.
#'  Lookout probabilities are returned by \code{lookout_prob} computed using a
#'  Generalized Pareto Distribution estimated from the kde scores and applied
#'  to the leave-one-out kde scores.
#' @param y Numerical vector of data
#' @param h Bandwidth. Dfault: \code{\link[stats]{bw.nrd}(y)}
#' @param binned Should binning be used to speed up calculation? Default: FALSE
#' @param loo Should leave-one-scores be returned? Default: FALSE
#' @return Numerical vector containing kde scores
#' @author Rob J Hyndman
#' @examples
#' y <- c(rnorm(9), 5)
#' kde_scores(y)
#' lookout_prob(y)
#' @export
#' @rdname kde_scores
#' @seealso
#'  \code{\link[stats]{bandwidth}}
#'  \code{\link[ks]{kde}}
#' @importFrom stats bw.nrd quantile
#' @importFrom ks kde
#' @importFrom evd fpot pgpd
kde_scores <- function(y, h=stats::bw.nrd(y), binned=FALSE, loo=FALSE) {
  -log(pmax(0, ks::kde(y, h = h,  binned=binned, eval.points = y)$estimate -
    loo/(length(y)*h*sqrt(2*pi))))
}

#' @rdname kde_scores
#' @export

lookout_prob <- function(y, h=stats::bw.nrd(y), binned=FALSE) {
  fi <- ks::kde(y, h = h,  binned=binned, eval.points = y)$estimate
  scores <- -log(pmax(0, fi))
  loo_scores <- -log(pmax(0, fi - 1/(length(y)*h*sqrt(2*pi))))
  threshold <- quantile(y, prob = 0.90)
  if(sum(y > threshold) <= 3)
    stop("Not enough data to fit a POT model")
  gpd <- evd::fpot(y, threshold = threshold)$estimate
  evd::pgpd(loo_scores, loc = threshold,
            scale = gpd["scale"], shape = gpd["shape"], lower.tail = FALSE)
}
