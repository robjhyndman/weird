#' @title Kernel density estimate scores
#' @description Compute kernel density estimate scores,
#'  defined as minus the log of the kernel density estimate at each observation.
#'  A Gaussian kernel is used, with default h given by a normal reference rule.
#' @param y Numerical vector of data
#' @param h Bandwidth. Dfault: \code{\link[stats]{bw.nrd}(y)}
#' @param binned Should binning be used to speed up calculation? Default: FALSE
#' @param loo Should leave-one-scores be returned? Default: FALSE
#' @return Numerical vector containing kde scores
#' @author Rob J Hyndman
#' @examples
#' kde_scores(c(rnorm(9),5))
#' @export
#' @rdname kde_scores
#' @seealso
#'  \code{\link[stats]{bandwidth}}
#'  \code{\link[ks]{kde}}
#' @importFrom stats bw.nrd
#' @importFrom ks kde
kde_scores <- function(y, h=stats::bw.nrd(y), binned=FALSE, loo=FALSE) {
  -log(pmax(0, ks::kde(y, h = h,  binned=binned, eval.points = y)$estimate -
    loo/(length(y)*h*sqrt(2*pi))))
}
