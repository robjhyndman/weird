#' Statistical tests for anomalies using Grubbs' test and Dixon's test
#'
#' Grubbs' test (proposed in 1950) identifies possible anomalies in univariate
#' data using z-scores assuming the data come from a normal distribution.
#'
#' @details The test is based on z-scores, and a point is identified as an
#' anomaly when the associated absolute z-score is greater than a threshold value.
#' A vector of logical values is returned, where \code{TRUE} indicates an anomaly.
#' This version of Grubbs' test looks for outliers anywhere in the sample.
#' Grubbs' original test came in several variations which looked for one outlier,
#' or two outliers in one tail, or two outliers on opposite tails. These variations
#' are implemented in the \code{\link[outliers]{grubbs.test}} function.
#' @references Grubbs, F. E. (1950). Sample criteria for testing outlying observations.
#' *Annals of Mathematical Statistics*, 21(1), 27–58.
#'
#' @author Rob J Hyndman
#' @param y numerical vector of observations
#' @param alpha size of the test.
#' @seealso \code{\link[outliers]{grubbs.test}}, \code{\link[outliers]{dixons.test}}
#' @examples
#' y <- c(rnorm(1000), 3:6)
#' tibble(y = y) |> filter(grubbs_anomalies(y))
#' @export

grubbs_anomalies <- function(y, alpha=0.05) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  n <- length(y)
  t2 <- qt(1 - alpha / (2 * n), n - 2)
  threshold <- (n - 1) / sqrt(n) * sqrt(t2^2 / (n - 2 + t2^2))
  return(abs(z) > threshold)
}

grubbs_test <- function(y, ...) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  n <- length(y)
  maxz <- max(abs(z))
  alpha <- c(10^(-(7:2)), seq(0.02,0.50, by=0.01))
  t2 <- qt(1 - alpha / (2 * n), n - 2)
  threshold <- (n - 1) / sqrt(n) * sqrt(t2^2 / (n - 2 + t2^2))
  p <-

  pval <- pt(maxz, n-2)
  output <- list(statistic = c(maxz = max(abs(z))), alternative = alt, p.value = pval,
                 method = "Dixon test for outliers", data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)

}
