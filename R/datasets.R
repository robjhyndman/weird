#' Cricket batting data for international test players
#'
#' A dataset containing career batting statistics for all international test
#' players (men and women) up to 6 October 2021.
#'
#' @format A data frame with 3754 rows and 15 variables:
#' \describe{
#'   \item{Player}{Player name in form of "initials surname"}
#'   \item{Country}{Country played for}
#'   \item{Start}{First year of test playing career}
#'   \item{End}{Last year of test playing career}
#'   \item{Matches}{Number of matches played}
#'   \item{Innings}{Number of innings batted}
#'   \item{NotOuts}{Number of times not out}
#'   \item{Runs}{Total runs scored}
#'   \item{HighScore}{Highest score in an innings}
#'   \item{HighScoreNotOut}{Was highest score not out?}
#'   \item{Average}{Batting average at end of career}
#'   \item{Hundreds}{Total number of 100s scored}
#'   \item{Fifties}{Total number of 50s scored}
#'   \item{Ducks}{Total number of 0s scored}
#'   \item{Gender}{"Men" or "Women"}
#' }
#' @return Data frame
#' @examples
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   select(Player, Country, Matches, Runs, Average, Hundreds, Fifties, Ducks) |>
#'   arrange(desc(Average))
#' @source \url{https://www.espncricinfo.com}
"cricket_batting"

#' Old faithful eruption data
#'
#' A data set containing data on recorded eruptions of the Old Faithful Geyser
#' in Yellowstone National Park, Wyoming, USA, from
#' 1 January 2015 to 1 October 2021.
#' Recordings are incomplete, especially during the winter months when observers
#' may not be present.
#'
#' @format A data frame with 2261 rows and 3 columns:
#' \describe{
#'   \item{time}{Time eruption started}
#'   \item{duration}{Duration of eruption in seconds}
#'   \item{waiting}{Time to the following eruption}
#' }
#' @return Data frame
#' @examples
#' oldfaithful |>
#'  filter(duration < 7000, waiting < 7000) |>
#'  ggplot(aes(x = duration, y = waiting)) +
#'  geom_point()
#' @source \url{https://geysertimes.org}
"oldfaithful"

#' Wine prices and points
#'
#' A data set containing data on wines from 44 countries, taken from *Wine Enthusiast Magazine*
#' during the week of 15 June 2017.
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
#' @examples
#' wine_reviews |>
#'  ggplot(aes(x = points, y = price)) +
#'  geom_jitter(height = 0, width = 0.2, alpha = 0.1) +
#'  scale_y_log10()
#' @source \url{https://kaggle.com}
"wine_reviews"

#' Multivariate standard normal data
#'
#' A synthetic data set containing 1000 observations on 10 variables generated
#' from independent standard normal distributions.
#'
#' @format A data frame with 1000 rows and 10 columns.
#' @return Data frame
#' @examples
#' n01
"n01"
