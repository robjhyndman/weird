library(tidyverse)

## Cricket batting data

library(cricketdata)
cricket_batting_men <- fetch_cricinfo(matchtype = "test", sex = "men", activity = "batting", type = "career")
cricket_batting_women <- fetch_cricinfo(matchtype = "test", sex = "women", activity = "batting", type = "career")
cricket_batting <- bind_rows(
  cricket_batting_men |> mutate(Gender = "Men"),
  cricket_batting_women |> mutate(Gender = "Women")
) |>
  mutate(Player = stringr::str_trim(Player)) |>
  arrange(Start, Country, Player)

usethis::use_data(cricket_batting, overwrite = TRUE)

## Old Faithful geyser data
file <- tempfile("oldfaithful", fileext = ".tsv.gz")
download.file(
  "https://geysertimes.org/archive/geysers/Old_Faithful_eruptions.tsv.gz",
  file
)
oldfaithful <- readr::read_tsv(file) |>
  arrange(eruption_time_epoch) |>
  mutate(
    # Convert epoch time to POSIXct
    time = as.POSIXct(eruption_time_epoch, origin = "1970-01-01", tz = "UTC"),
    # Compute waiting times
    waiting = lead(eruption_time_epoch) - eruption_time_epoch
  ) |>
  filter(
    eruption_time_epoch > as.numeric(as.POSIXct("2017-01-01", tz = "UTC")),
    eruption_time_epoch < as.numeric(as.POSIXct("2024-01-01", tz = "UTC")),
    duration_seconds != "NULL",
    waiting > 600,
    waiting < 7200
  ) |>
  mutate(ds = readr::parse_number(duration_seconds)) |>
  select(time, recorded_duration = duration, duration = ds, waiting)

usethis::use_data(oldfaithful, overwrite = TRUE)

# Wine data

wine_reviews <- readr::read_csv(here::here("data-raw/winemag-data-130k-v2.csv")) |>
  filter(!is.na(points), !is.na(price)) |>
  mutate(
    year = as.numeric(stringr::str_extract(title, "(198|199|200|201)\\d")),
    region_1 = if_else(is.na(region_2), region_1, region_2)
  ) |>
  select(country, state = province, region = region_1, winery, variety, points, price, year) |>
  distinct()

# usethis::use_data(wine_reviews, overwrite = TRUE)
# Save to data-raw folder for downloading at run time. This circumvents the
## non-ASCII data rule of CRAN.
saveRDS(wine_reviews, here::here("data-raw/wine_reviews.rds"))

# Synthetic data
set.seed(1)
n01 <- matrix(rnorm(1e4), ncol = 10) |> as_tibble(.name_repair = "unique")
colnames(n01) <- paste("v", seq(10), sep = "")
usethis::use_data(n01, overwrite = TRUE)
