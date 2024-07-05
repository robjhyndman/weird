library(tsibble)
library(dplyr)

fr_mortality <- vital::read_hmd_files(here::here("data-raw/Mx_1x1.txt")) |>
  filter(Sex != "Total", Year < 2000) |>
  vital::collapse_ages(max_age = 85) |>
  as_tibble() |>
  select(Year, Age, Sex, Mortality)

usethis::use_data(fr_mortality, overwrite = TRUE)
