#' Identify anomalies using the Hampel filter
#'
#' The Hampel filter is designed to find anomalies in time series data using
#' mean absolute deviations in the vicinity of each observation.
#' @details First, a moving median is calculated using windows of size
#' `2 * bandwidth + 1`. Then the median absolute deviations from
#' this moving median are calculated in the same moving windows.
#' A point is declared an anomaly if its MAD is value is more than `k` standard
#' deviations. The MAD is converted to a standard deviation using MAD * 1.482602,
#' which holds for normally distributed data.
#' The first `bandwidth` and last `bandwidth` observations cannot
#' be declared anomalies.
#' @param y numeric vector containing time series
#' @param bandwidth integer width of the window around each observation
#' @param k numeric number of standard deviations to declare an outlier
#' @return logical vector identifying which observations are anomalies.
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 9.2,
#' \url{https://OTexts.com/weird/}.
#' @author Rob J Hyndman
#' @examples
#' set.seed(1)
#' df <- tibble(
#'   time = seq(41),
#'   y = c(rnorm(20), 5, rnorm(20))
#' ) |>
#'   mutate(hampel = hampel_anomalies(y, bandwidth = 3, k = 4))
#' df |> ggplot(aes(x = time, y = y)) +
#'   geom_line() +
#'   geom_point(data = df |> filter(hampel), col = "red")
#' @export

hampel_anomalies <- function(y, bandwidth, k = 3) {
  if (abs(bandwidth - round(bandwidth)) > 1e-8) {
    stop("Bandwidth must be an integer")
  }
  bandwidth <- as.integer(round(bandwidth))
  n <- length(y)
  # Running medians
  m <- stats::runmed(
    y,
    2 * bandwidth + 1,
    endrule = "keep",
    na.action = "na.omit"
  )
  diff <- abs(y - m)
  # Set MAD to Inf so end points are not considered outliers
  mad <- rep(Inf, n)
  # Running MADs
  for (i in (bandwidth + 1):(n - bandwidth)) {
    mad[i] <- stats::median(
      abs(y[(i - bandwidth):(i + bandwidth)] - m[i]),
      na.rm = TRUE
    )
  }
  # Find outliers
  return(diff > mad * k * 1.482602)
}
