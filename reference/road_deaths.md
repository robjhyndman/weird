# Australian road deaths

Every death from a road crash in Australia from January 1989 to December
2025, from the Australian Road Deaths Database (ARDD). Each row is a
person killed; people killed in the same crash share a `crash_id`. A
death is counted if the crash occurred on a public road, was
unintentional, and the person died within 30 days.

## Usage

``` r
road_deaths
```

## Format

A data frame with 58168 rows and 20 columns:

- crash_id:

  Crash identifier

- state:

  State or territory where the crash occurred

- year:

  Year of crash

- month:

  Month of crash (1–12)

- day_of_week:

  Day of the week of the crash

- time:

  Time of crash in hours after midnight (e.g. 14.5 is 2:30pm)

- crash_type:

  Single or multiple vehicle crash

- bus:

  Was a bus involved?

- heavy_rigid_truck:

  Was a heavy rigid truck involved?

- articulated_truck:

  Was an articulated truck involved?

- speed_limit:

  Posted speed limit at the crash location (km/h)

- road_user:

  Road user type of the person killed

- gender:

  Sex of the person killed

- age:

  Age of the person killed (years)

- remoteness:

  ABS remoteness area of the crash location (ASGS 2021)

- sa4:

  ABS Statistical Area Level 4 of the crash location (ASGS 2021)

- lga:

  Local government area of the crash location (ASGS 2021)

- road_type:

  Type of road

- christmas:

  Did the crash occur in the 12 days from 23 December?

- easter:

  Did the crash occur in the 5 days from the Thursday before Good
  Friday?

## Source

Bureau of Infrastructure and Transport Research Economics (2026).
Australian Road Deaths Database, fatalities. Downloaded 9 October 2026.
Licensed under CC BY.
<https://catalogue.data.infrastructure.gov.au/dataset/australian-road-deaths-database>

## Value

Data frame

## Details

Missing values (coded as -9 or "Unknown" in the source) are `NA`. Some
variables were not recorded for early years: `heavy_rigid_truck` is
mostly missing before 2002, and `remoteness`, `sa4`, `lga` and
`road_type` are missing before 2014 and incomplete until 2017.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter
10, <https://OTexts.com/weird/text.html>.

## Examples

``` r
road_deaths |>
  ggplot(aes(x = age, fill = road_user)) +
  geom_histogram(binwidth = 1)
#> Warning: Removed 108 rows containing non-finite outside the scale range (`stat_bin()`).
```
