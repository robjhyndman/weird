library(readr)
library(dplyr)

files <- list.files(
  path = here::here("data-raw"),
  pattern = "^PRSA.*\\.csv$",
  full.names = TRUE
)
air_quality <- purrr::map_dfr(files, read_csv, col_types = cols()) |>
  janitor::clean_names() |>
  select(-no) |>
  rename(
    temperature = temp,
    pressure = pres,
    dew_point = dewp,
    rainfall = rain,
    wind_direction = wd,
    wind_speed = wspm
  ) |>
  select(station, year, month, day, hour, everything())

saveRDS(air_quality, here::here("data-raw/air_quality.rds"))
