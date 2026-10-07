# US weekly deaths and death rates from the Short-Term Mortality Fluctuations
# (STMF) series of the Human Mortality Database, https://www.mortality.org/
# File USAstmfout.csv downloaded January 2026.

library(dplyr)
library(tidyr)

ages <- c("0-14", "15-64", "65-74", "75-84", "85+", "Total")
stmf <- read.csv("data-raw/USAstmfout.csv", check.names = FALSE)
colnames(stmf) <- c(
  "CountryCode", "Year", "Week", "Sex",
  paste0("D_", ages), paste0("R_", ages),
  "Split", "SplitSex", "Forecast"
)
us_mortality <- stmf |>
  select(Year, Week, Sex, starts_with("D_"), starts_with("R_")) |>
  pivot_longer(
    c(starts_with("D_"), starts_with("R_")),
    names_to = c(".value", "Age"),
    names_sep = "_"
  ) |>
  rename(Deaths = D, Mortality = R) |>
  mutate(
    Year = as.integer(Year),
    Week = as.integer(Week),
    Sex = recode(Sex, f = "Female", m = "Male", b = "Total")
  ) |>
  arrange(Year, Week, Sex, Age) |>
  as_tibble()

usethis::use_data(us_mortality, overwrite = TRUE, compress = "xz")
