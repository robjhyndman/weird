#' Hampel filter
#'
#' The Hampel filter is designed to find outliers in time series data using
#' mean absolute deviations in the vicinity of each observation.
#' @details The median absolute deviation is done in a window of times within
#' `bandwidth` of each observation. A point is declared an outlier if its MAD
#' value is more than k standard deviations
#' @param y numeric vector containing time series
#' @param bandwidth integer width of the window around each observation
#' @param k numeric number of standard deviations to declare an outlier
#' @return numeric vector with outliers replaced by the median of the window.
#' @author Rob J Hyndman
#' @examples
#' set.seed(1)
#' tibble(
#'   time = seq(41),
#'   y = c(rnorm(20), 5, rnorm(20))
#' ) |>
#'   mutate(ystar = hampel(y, bandwidth = 3, k = 4)) |>
#'   ggplot(aes(x = time, y = y)) +
#'   geom_line(color = "red") +
#'   geom_line(aes(y = ystar))
#' @export

hampel <- function(y, bandwidth, k = 3) {
  n <- length(y)
  # Running medians
  m <- stats::runmed(y, 2 * bandwidth + 1, endrule = "keep", na.action = "na.omit")
  diff <- abs(y - m)
  # Set MAD to Inf so end points are not considered outliers
  mad <- rep(Inf, n)
  # Running MADs
  for (i in (bandwidth + 1):(n - bandwidth)) {
    mad[i] <- median(abs(y[(i - bandwidth):(i + bandwidth)] - m[i]), na.rm = TRUE)
  }
  # Find outliers
  outliers <- diff > mad * k * 1.482602
  # Replace outliers by medians
  y[outliers] <- m[outliers]
  return(y)
}
