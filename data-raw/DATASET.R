library(tidyverse)

## Cricket batting data

library(cricketdata)
cricket_batting_men <- fetch_cricinfo(matchtype="test", sex="men", activity="batting", type="career")
cricket_batting_women <- fetch_cricinfo(matchtype="test", sex="women", activity="batting", type="career")
cricket_batting <- bind_rows(
  cricket_batting_men %>% mutate(Gender = "Men"),
  cricket_batting_women %>% mutate(Gender = "Women")
)

usethis::use_data(cricket_batting, overwrite = TRUE)

## Old Faithful geyser data

library(geysertimes)
# Only do the following once
gt_get_data(dest_folder = here::here("data-raw"))
oldfaithful <- gt_load_eruptions(path = here::here("data-raw")) %>%
  filter(geyser == "Old Faithful", eruption_id == primary_id) %>%
  arrange(time) %>%
  mutate(
    # Compute waiting times
    waiting = as.numeric(lead(time) - time),
    # Omit waiting time if more than 12 hours -- seems unlikely
    #waiting = if_else(waiting > 12*60*60, NA_real_, waiting)
  ) %>%
  filter(
    as.Date(time) > "2015-01-01",
    !is.na(duration_seconds)
  ) %>%
  select(time, duration=duration_seconds, waiting)

usethis::use_data(oldfaithful, overwrite = TRUE)
