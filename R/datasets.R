#' Cricket batting data for international test players
#'
#' A dataset containing career batting statistics for all international test
#' players (men and women) up to February 2021.
#'
#' @format A data frame with 3719 rows and 15 variables:
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
#' @source \url{http://espncricinfo.com}
"cricket_batting"

#' Old faithful eruption data
#'
#' A data set containing data on recorded eruptions of Old Faithful since 2015.
#' Recordings are incomplete, especially during the winter months when observers
#' may not be present.
#'
#' @format A data frame with 1937 rows and 3 columns:
#' \describe{
#'   \item{time}{Time eruption started}
#'   \item{duration}{Duration of eruption in seconds}
#'   \item{waiting}{Time to the following eruption}
#' }
#' @source \url{http://geysertimes.org}
"oldfaithful"
