# Old faithful eruption data

A data set containing data on recorded eruptions of the Old Faithful
Geyser in Yellowstone National Park, Wyoming, USA, from 14 January 2017
to 29 December 2023. Recordings are incomplete, especially during the
winter months when observers may not be present.

## Usage

``` r
oldfaithful
```

## Format

A data frame with 2097 rows and 4 columns:

- time:

  Time eruption started

- recorded_duration:

  Duration of eruption as recorded

- duration:

  Duration of eruption in seconds

- waiting:

  Time to the following eruption in seconds

## Source

<https://geysertimes.org>

## Value

Data frame

## Examples

``` r
oldfaithful |>
  ggplot(aes(x = duration, y = waiting)) +
  geom_point()
```
