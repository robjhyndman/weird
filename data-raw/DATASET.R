## code to prepare `DATASET` dataset goes here

library(cricketdata)
library(tidyverse)
cricket_batting_men <- fetch_cricinfo(matchtype="test", sex="men", activity="batting", type="career")
cricket_batting_women <- fetch_cricinfo(matchtype="test", sex="women", activity="batting", type="career")
cricket_batting <- bind_rows(
  cricket_batting_men %>% mutate(Gender = "Men"),
  cricket_batting_women %>% mutate(Gender = "Women")
)

usethis::use_data(cricket_batting, overwrite = TRUE)
