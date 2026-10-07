#' Cricket batting data for international test players
#'
#' A dataset containing career batting statistics for all international test
#' players (men and women) up to 6 October 2025.
#'
#' @format A data frame with `r nrow(cricket_batting)` rows and
#' `r ncol(cricket_batting)` variables:
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
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 1.4,
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
#' @format A data frame with `r nrow(oldfaithful)` rows and
#' `r ncol(oldfaithful)` columns:
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
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @source \url{https://geysertimes.org}
"oldfaithful"

#' Multivariate standard normal data
#'
#' A synthetic data set containing `r nrow(n01)` observations on
#' `r ncol(n01)` variables generated
#' from independent standard normal distributions.
#'
#' @format A data frame with `r nrow(n01)` rows and `r ncol(n01)` columns.
#' @return Data frame
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @examples
#' n01
"n01"

#' French mortality rates by age and sex
#'
#' A data set containing French mortality rates between the years 1816 and 1999,
#' by age and sex.
#'
#' @format A data frame with `r nrow(fr_mortality)` rows and
#' `r ncol(fr_mortality)` columns.
#' @source Human Mortality Database \url{https://www.mortality.org}
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section 1.4,
#' \url{https://OTexts.com/weird/}.
#' @return Data frame
#' @examples
#' fr_mortality
"fr_mortality"

#' Gun ownership and homicide rates by country
#'
#' A data set containing gun ownership rates and homicide rates for 2017 for
#' various countries around the world. The gun ownership rates are the number of guns owned by civilians per 100 people. The homicide rates are the number of homicides per 100,000 people where the weapon was a firearm.
#'
#' @format A data frame with `r nrow(gun_deaths)` rows and
#' `r ncol(gun_deaths)` columns:
#' \describe{
#'   \item{country}{Country name}
#'  \item{region}{World region according to Our World in Data}
#'   \item{gun_ownership_rate}{Gun ownership rate (number of guns owned by civilians per 100 people)}
#'   \item{homicide_rate}{Homicide rate (number of homicides per 100,000 people where the weapon was a firearm)}
#' }
#' @source World Population Review \url{https://worldpopulationreview.com/country-rankings/gun-ownership-by-country} and \url{https://ourworldindata.org/grapher/homicide-rates-from-firearms}
#' @return Data frame
#' @examples
#' gun_deaths
"gun_deaths"

#' US Senate co-voting networks
#'
#' Co-voting networks for the US Senate from the 40th to the 113th Congress
#' (1867 to 2015). Each Congress gives an undirected network in which each
#' node is a senator, and an edge connects two senators if they voted the same
#' way (both yea or both nay) on at least 80% of the bills for which they were
#' both present. The data are taken from Lee, Li and Wilson (2020).
#' Although they state that Independent senators were excluded, a few
#' Independent and minor-party senators are included, coded as Democrat or
#' Republican.
#'
#' `us_senate_edges` contains the edges of every network, and `us_senate_members`
#' contains the name, state and party of each senator. Senators are numbered
#' from 1 within each Congress, so the same number refers to different
#' senators in different Congresses; use `icpsr` to follow a senator across
#' Congresses. From the 84th Congress onwards, node 1 is the President, whose
#' announced positions on bills are treated as votes.
#'
#' @format `us_senate_edges` is a data frame with `r nrow(us_senate_edges)` rows and
#' `r ncol(us_senate_edges)` columns:
#' \describe{
#'   \item{congress}{Congress number}
#'   \item{from}{Node number of one senator}
#'   \item{to}{Node number of the other senator}
#' }
#' `us_senate_members` is a data frame with `r nrow(us_senate_members)` rows and
#' `r ncol(us_senate_members)` columns:
#' \describe{
#'   \item{congress}{Congress number}
#'   \item{node}{Node number of the senator}
#'   \item{name}{Name of the senator (surname first)}
#'   \item{state}{Two-letter state abbreviation ("USA" for the President)}
#'   \item{party}{"Democrat" or "Republican"}
#'   \item{icpsr}{ICPSR identifier of the senator, as used by Voteview}
#' }
#' @return Data frame
#' @examples
#' # Number of co-voting edges in each Congress
#' us_senate_edges |>
#'   count(congress)
#' # Party composition of the 100th Congress
#' us_senate_members |>
#'   filter(congress == 100) |>
#'   count(party)
#' # Congresses in which Patrick Leahy served
#' us_senate_members |>
#'   filter(icpsr == 14307)
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter 12,
#' \url{https://OTexts.com/weird/}.
#' @source Lee, J., Li, G., & Wilson, J. D. (2020). Varying-coefficient models
#' for dynamic networks. *Computational Statistics & Data Analysis*, 152, 107052.
#' \doi{10.1016/j.csda.2020.107052}. Data available from
#' \url{https://github.com/jihuilee/VCERGM}.
#'
#' Names, states and ICPSR identifiers from Lewis, J. B., Poole, K.,
#' Rosenthal, H., Boche, A., Rudkin, A., & Sonnet, L. Voteview: Congressional
#' roll-call votes database. \url{https://voteview.com/}
#' @name us_senate
"us_senate_edges"

#' @rdname us_senate
"us_senate_members"

#' US weekly mortality
#'
#' Weekly deaths and death rates in the USA, by sex and age group, from the
#' second week of 2015 to the 50th week of 2025. The data are from the
#' Short-Term Mortality Fluctuations (STMF) series of the Human Mortality
#' Database. The most recent weeks are subject to reporting delays.
#'
#' @format A data frame with `r nrow(us_mortality)` rows and
#' `r ncol(us_mortality)` columns:
#' \describe{
#'   \item{Year}{Year}
#'   \item{Week}{ISO week of the year}
#'   \item{Sex}{"Female", "Male" or "Total"}
#'   \item{Age}{Age group: "0-14", "15-64", "65-74", "75-84", "85+" or "Total"}
#'   \item{Deaths}{Number of deaths}
#'   \item{Mortality}{Weekly death rate: deaths divided by population exposure}
#' }
#' @return Data frame
#' @examples
#' us_mortality |>
#'   filter(Sex == "Total", Age != "Total") |>
#'   mutate(time = Year + (Week - 1) / 52) |>
#'   ggplot(aes(x = time, y = Mortality, colour = Age)) +
#'   geom_line() +
#'   scale_y_log10()
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter 10,
#' \url{https://OTexts.com/weird/}.
#' @source Human Mortality Database. Max Planck Institute for Demographic
#' Research (Germany), University of California, Berkeley (USA), and French
#' Institute for Demographic Studies (France). \url{https://www.mortality.org}
"us_mortality"

#' Fashion-MNIST sneakers
#'
#' All 1000 images of sneakers from the Fashion-MNIST test set, together with
#' 10 images from other classes of varying similarity to sneakers: four ankle
#' boots, three sandals, two bags and one pair of trousers. Each image is
#' 28 x 28 pixels.
#'
#' @format A data frame with `r nrow(fashion)` rows and
#' `r ncol(fashion)` columns:
#' \describe{
#'   \item{id}{Index of the image in the Fashion-MNIST test set}
#'   \item{label}{Class of the image (a factor with the 10 Fashion-MNIST classes)}
#'   \item{planted}{`TRUE` for the 10 images that are not sneakers}
#'   \item{pixels}{A 1010 x 784 integer matrix of pixel intensities, from 0
#'     (white) to 255 (black), with one row per image. Each row contains the
#'     28 x 28 pixels of an image, stored row by row, so
#'     `matrix(pixels[i, ], nrow = 28, byrow = TRUE)` gives image `i`.}
#' }
#' @return Data frame
#' @examples
#' fashion |>
#'   count(label)
#' @references Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter 13,
#' \url{https://OTexts.com/weird/}.
#' @source Xiao, H., Rasul, K., & Vollgraf, R. (2017). Fashion-MNIST: a novel
#' image dataset for benchmarking machine learning algorithms.
#' \url{https://github.com/zalandoresearch/fashion-mnist} (MIT licence)
"fashion"
