#' Air quality data for 12 Beijing monitoring stations from 2013 to 2017
#'
#' Hourly air quality measurements from 12 monitoring stations across Beijing,
#' China, from 1 March 2013 to 28 February 2017. The data are downloaded and
#' returned.
#'
#' @format A data frame with 420,768 rows and 17 columns:
#' \describe{
#'   \item{station}{Name of the monitoring station}
#'   \item{year}{Year of measurement}
#'   \item{month}{Month of measurement}
#'   \item{day}{Day of measurement}
#'   \item{hour}{Hour of measurement (0--23)}
#'   \item{pm2_5}{Particulate matter with diameter less than 2.5 micrometers
#'     (micrograms per cubic meter)}
#'   \item{pm10}{Particulate matter with diameter less than 10 micrometers
#'     (micrograms per cubic meter)}
#'   \item{so2}{Sulfur dioxide concentration (micrograms per cubic meter)}
#'   \item{no2}{Nitrogen dioxide concentration (micrograms per cubic meter)}
#'   \item{co}{Carbon monoxide concentration (micrograms per cubic meter)}
#'   \item{o3}{Ozone concentration (micrograms per cubic meter)}
#'   \item{temperature}{Temperature (degrees Celsius)}
#'   \item{pressure}{Atmospheric pressure (hPa)}
#'   \item{dew_point}{Dew point temperature (degrees Celsius)}
#'   \item{rainfall}{Rainfall (millimeters)}
#'   \item{wind_direction}{Wind direction}
#'   \item{wind_speed}{Wind speed (meters per second)}
#' }
#' @return Data frame
#' @references Hyndman, R J (2026) *That's weird: Anomaly detection using R*,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' \dontrun{
#' air_quality <- fetch_air_quality()
#' air_quality |>
#'   filter(station == "Aotizhongxin") |>
#'   ggplot(aes(x = temperature, y = pm2_5)) +
#'   geom_point(alpha = 0.1)
#' }
#' @source UCI Machine Learning Repository
#' \url{https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data}
#' @aliases air_quality
#' @export
fetch_air_quality <- function() {
  dest_folder <- tempdir()
  dest_file <- paste0(dest_folder, "/air_quality.rds")
  if (!file.exists(dest_file)) {
    utils::download.file(
      url = "https://github.com/robjhyndman/weird/raw/main/data-raw/air_quality.rds",
      destfile = dest_file,
      mode = "wb"
    )
  }
  readRDS(dest_file)
}
