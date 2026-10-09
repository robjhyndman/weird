# Australian Road Deaths Database (ARDD), fatalities table
# Bureau of Infrastructure and Transport Research Economics (BITRE), CC BY
# https://catalogue.data.infrastructure.gov.au/dataset/australian-road-deaths-database
# File downloaded 9 October 2026 (data current to August 2026) from
# https://datahub.roadsafety.gov.au/sites/default/files/documents/bitre_fatalities_aug2026.xlsx

library(dplyr)

ardd <- readxl::read_excel(
  here::here("data-raw/bitre_fatalities_aug2026.xlsx"),
  sheet = "BITRE_Fatality",
  skip = 4,
  col_types = "text"
)

# Missing values are coded as "-9" or "Unknown" (and "99:99:99" for time,
# which the data dictionary does not mention)
unknown <- function(x) if_else(x %in% c("-9", "Unknown", "99:99:99"), NA_character_, x)
yes_no <- function(x) c(Yes = TRUE, No = FALSE)[unknown(x)] |> unname()

road_deaths <- ardd |>
  mutate(across(everything(), unknown)) |>
  transmute(
    crash_id = `Crash ID`,
    state = factor(State, levels = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")),
    year = as.integer(Year),
    month = as.integer(Month),
    day_of_week = factor(
      Dayweek,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    # Hours after midnight
    time = as.integer(substr(Time, 1, 2)) + as.integer(substr(Time, 4, 5)) / 60,
    crash_type = factor(`Crash Type`, levels = c("Single", "Multiple")),
    bus = yes_no(`Bus Involvement`),
    heavy_rigid_truck = yes_no(`Heavy Rigid Truck Involvement`),
    articulated_truck = yes_no(`Articulated Truck Involvement`),
    speed_limit = as.integer(`Speed Limit`),
    road_user = factor(
      `Road User`,
      levels = c(
        "Driver",
        "Passenger",
        "Pedestrian",
        "Motorcycle rider",
        "Motorcycle pillion passenger",
        "Pedal cyclist"
      )
    ),
    gender = factor(Gender, levels = c("Male", "Female")),
    age = as.integer(Age),
    remoteness = factor(
      `National Remoteness Areas 2021`,
      levels = c(
        "Major Cities of Australia",
        "Inner Regional Australia",
        "Outer Regional Australia",
        "Remote Australia",
        "Very Remote Australia"
      )
    ),
    sa4 = `SA4 Name 2021`,
    lga = `National LGA Name 2021`,
    road_type = factor(
      `National Road Type`,
      levels = c(
        "National or State Highway",
        "Arterial Road",
        "Sub-arterial Road",
        "Collector Road",
        "Local Road",
        "Access road",
        "Busway",
        "Pedestrian Thoroughfare"
      )
    ),
    christmas = yes_no(`Christmas Period`),
    easter = yes_no(`Easter Period`)
  ) |>
  # Recent months are preliminary and get revised; stop at the end of 2025
  filter(year <= 2025) |>
  arrange(year, month, crash_id)

# Every non-missing source value must survive the recoding
ardd <- filter(ardd, as.integer(Year) <= 2025)
stopifnot(
  !anyNA(road_deaths$state),
  !anyNA(road_deaths$day_of_week),
  sum(is.na(road_deaths$time)) == sum(ardd$Time == "99:99:99"),
  sum(is.na(road_deaths$crash_type)) == sum(ardd$`Crash Type` == "Unknown"),
  sum(is.na(road_deaths$road_user)) == sum(ardd$`Road User` == "Unknown"),
  sum(is.na(road_deaths$road_type)) == sum(ardd$`National Road Type` == "Unknown"),
  sum(is.na(road_deaths$remoteness)) ==
    sum(ardd$`National Remoteness Areas 2021` == "Unknown")
)

usethis::use_data(road_deaths, overwrite = TRUE, compress = "xz")
