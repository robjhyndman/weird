#' Cricket batting data for international test players
#'
#' A dataset containing career batting statistics for all international test
#' players (men and women) up to 6 October 2025.
#'
#' @format A data frame with 3968 rows and 15 variables:
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
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @return Data frame
#' @examples
#' cricket_batting |>
#'   filter(Innings > 20) |>
#'   select(Player, Country, Matches, Runs, Average, Hundreds, Fifties, Ducks) |>
#'   arrange(desc(Average))
#' @source \url{https://www.espncricinfo.com/}
"cricket_batting"

#' Old faithful eruption data
#'
#' A data set containing data on recorded eruptions of the Old Faithful Geyser
#' in Yellowstone National Park, Wyoming, USA, from
#' 14 January 2017 to 29 December 2023.
#' Recordings are incomplete, especially during the winter months when observers
#' may not be present.
#'
#' @format A data frame with 2097 rows and 4 columns:
#' \describe{
#'   \item{time}{Time eruption started}
#'   \item{recorded_duration}{Duration of eruption as recorded}
#'   \item{duration}{Duration of eruption in seconds}
#'   \item{waiting}{Time to the following eruption in seconds}
#' }
#' @return Data frame
#' @examples
#' oldfaithful |>
#'   ggplot(aes(x = duration, y = waiting)) +
#'   geom_point()
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @source \url{https://geysertimes.org}
"oldfaithful"

#' Multivariate standard normal data
#'
#' A synthetic data set containing 1000 observations on 10 variables generated
#' from independent standard normal distributions.
#'
#' @format A data frame with 1000 rows and 10 columns.
#' @return Data frame
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' n01
"n01"

#' French mortality rates by age and sex
#'
#' A data set containing French mortality rates between the years 1816 and 1999,
#' by age and sex.
#'
#' @format A data frame with 31,648 rows and 4 columns.
#' @source Human Mortality Database \url{https://www.mortality.org}
#' @references Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @return Data frame
#' @examples
#' fr_mortality
"fr_mortality"
