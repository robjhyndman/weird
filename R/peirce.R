#' Anomalies according to Peirce's and Chauvenet's criteria
#'
#' Peirce's criterion and Chauvenet's criterion were both proposed in the 1800s
#' as a way of determining what observations should be rejected in a univariate sample.
#'
#' @details These functions take a univariate sample `y` and return a logical
#' vector indicating which observations should be considered anomalies according
#' to either Peirce's criterion or Chauvenet's criterion.
#' @references Peirce, B. (1852). Criterion for the rejection of doubtful observations.
#' *The Astronomical Journal*, 2(21), 161–163.
#' @references Chauvenet, W. (1863). 'Method of least squares'. Appendix to
#' *Manual of Spherical and Practical Astronomy*, Vol.2, Lippincott, Philadelphia, pp.469-566.
#'
#' @author Rob J Hyndman, K Thomsen, C Dardis, S Muller
#' @param y numerical vector of observations
#' @examples
#' y <- rnorm(1000)
#' tibble(y = y) %>% dplyr::filter(peirce_anomalies(y))
#' tibble(y = y) %>% dplyr::filter(chauvenet_anomalies(y))
#' @export

peirce_anomalies <- function(y) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  return(abs(z) > peirce_threshold(length(y)))
}

peirce_threshold <- function(n, k = 1L, m = 1L) {
  # method by K. Thomsen (2008)
  # written by C. Dardis and S. Muller (2012)
  # modified by Rob J Hyndman (2021)
  # Available online: https://r-forge.r-project.org/R/?group_id=1473

  # Check we have enough observations
  if ((n - m - k) <= 0)
    return(NaN)

  x <- min(1, sqrt((n - m) / k) - 1e-10)
  # Log of Gould's equation B:
  LnQN <- k * log(k) + (n - k) * log(n - k) - n * log(n)
  # Gould's equation D:
  R1 <- 2 * exp((x^2 - 1) / 2) * stats::pnorm(x, lower.tail = FALSE)
  # Gould's equation A' solved for R w/ Lambda substitution:
  R2 <- exp((LnQN - 0.5 * (n - k) * log((n - m - k * x^2) / (n - m - k))) / k)
  # Equate the two R equations:
  R1d <- x * R1 - sqrt(2 / pi / exp(1))
  R2d <- x * (n - k) / (n - m - k * x^2) * R2
  # Update x:
  oldx <- x
  x <- oldx - (R1 - R2) / (R1d - R2d)
  # Loop until convergence:
  while (abs(x - oldx) >= n * 2e-16) {
    R1 <- 2 * exp((x^2 - 1) / 2) * stats::pnorm(x, lower.tail = FALSE)
    R2 <- exp((LnQN - 0.5 * (n - k) * log((n - m - k * x^2) / (n - m - k))) / k)
    R1d <- x * R1 - sqrt(2 / pi / exp(1))
    R2d <- x * (n - k) / (n - m - k * x^2) * R2
    oldx <- x
    x <- oldx - (R1 - R2) / (R1d - R2d)
  }
  return(x)
}

#' @rdname peirce_anomalies
#' @export

chauvenet_anomalies <- function(y) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  2 * length(y) * stats::pnorm(abs(z) * sqrt(2), lower.tail = FALSE) <= 0.5
}
