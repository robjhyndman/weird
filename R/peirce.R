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
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 4.3,
#' \url{https://OTexts.com/weird/}.
#' @return A logical vector
#' @author Rob J Hyndman
#' @param y numerical vector of observations
#' @examples
#' y <- rnorm(1000)
#' tibble(y = y) |> filter(peirce_anomalies(y))
#' tibble(y = y) |> filter(chauvenet_anomalies(y))
#' @export

peirce_anomalies <- function(y) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  return(abs(z) > peirce_threshold(length(y)))
}

# Threshold based on Gould's paper when there are m=1 unknown quantities
# and n=1 suspicious observations
peirce_threshold <- function(n) {
  # Check we have enough observations
  if ((n - 2) <= 0) {
    return(NaN)
  }
  # Initialize
  x <- 1
  oldx <- Inf
  root2pie <- sqrt(2 / pi / exp(1))
  # Eq (B) after taking logs
  LnQN <- (n - 1) * log(n - 1) - n * log(n)
  # Loop until convergence
  while (abs(x - oldx) >= n * .Machine$double.eps) {
    # Eq (D)
    R1 <- 2 * exp(0.5 * (x^2 - 1)) * stats::pnorm(x, lower.tail = FALSE)
    # Eq (A') after taking logs and solving for R (plug in lambda from top of page)
    R2 <- exp(LnQN - (n - 1) * 0.5 * log((n - 1 - x^2) / (n - 2)))
    # Find derivatives wrt x
    R1d <- x * R1 - root2pie
    R2d <- x * (n - 1) / (n - 1 - x^2) * R2
    # Update x accordingly
    oldx <- x
    x <- oldx - (R1 - R2) / (R1d - R2d)
  }
  return(x)
}

#' @rdname peirce_anomalies
#' @export

chauvenet_anomalies <- function(y) {
  z <- (y - mean(y, na.rm = TRUE)) / stats::sd(y, na.rm = TRUE)
  return(abs(z) > stats::qnorm(1 - 0.25 / length(y)))
}
