#' Wine prices and points
#'
#' A data set containing data on wines from 44 countries, taken from *Wine Enthusiast Magazine*
#' during the week of 15 June 2017. The data are downloaded and returned.
#'
#' @format A data frame with 110,203 rows and 8 columns:
#' \describe{
#'   \item{country}{Country of origin}
#'   \item{state}{State or province of origin}
#'   \item{region}{Region of origin}
#'   \item{winery}{Name of vineyard that made the wine}
#'   \item{variety}{Variety of grape}
#'   \item{points}{Points allocated by WineEnthusiast reviewer on a scale of 0-100}
#'   \item{price}{Price of a bottle of wine in $US}
#'   \item{year}{Year of wine extracted from `title`}
#' }
#' @return Data frame
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' \dontrun{
#' wine_reviews <- fetch_wine_reviews()
#' wine_reviews |>
#'   ggplot(aes(x = points, y = price)) +
#'   geom_jitter(height = 0, width = 0.2, alpha = 0.1) +
#'   scale_y_log10()
#' }
#' @source \url{https://www.kaggle.com/}
#' @aliases wine_reviews
#' @export
fetch_wine_reviews <- function() {
  # Download to temporary file to avoid repeated downloads in the same session
  dest_folder <- tempdir()
  dest_file <- paste0(dest_folder, "/wine_reviews.rds")
  if (!file.exists(dest_file)) {
    utils::download.file(
      url = "https://github.com/robjhyndman/weird/raw/main/data-raw/wine_reviews.rds",
      destfile = dest_file
    )
  }
  readRDS(dest_file)
}
