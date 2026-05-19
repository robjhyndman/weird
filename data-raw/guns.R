library(readr)
library(dplyr)

gun_owners <- read_csv(here::here(
  "data-raw/gun-ownership-by-country-2026.csv"
)) |>
  select(country, gun_ownership_rate = GunOwnershipCivilianOwnershipRate_2017)
homicide_rates <- read_csv(here::here(
  "data-raw/homicide-rates-from-firearms.csv"
)) |>
  filter(Year == 2017) |>
  select(
    country = Entity,
    homicide_rate = `Homicide rate per 100,000 population  where the weapon was a firearm`,
    region = `World region according to OWID`
  )

gun_deaths <- gun_owners |>
  inner_join(homicide_rates, by = "country")

usethis::use_data(gun_deaths, overwrite = TRUE)
