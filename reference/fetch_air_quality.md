# Air quality data for 12 Beijing monitoring stations from 2013 to 2017

Hourly air quality measurements from 12 monitoring stations across
Beijing, China, from 1 March 2013 to 28 February 2017. The data are
downloaded and returned.

## Usage

``` r
fetch_air_quality()
```

## Format

A data frame with 420,768 rows and 17 columns:

- year:

  Year of measurement

- month:

  Month of measurement

- day:

  Day of measurement

- hour:

  Hour of measurement (0–23)

- pm2_5:

  Particulate matter with diameter less than 2.5 micrometers (micrograms
  per cubic meter)

- pm10:

  Particulate matter with diameter less than 10 micrometers (micrograms
  per cubic meter)

- so2:

  Sulfur dioxide concentration (micrograms per cubic meter)

- no2:

  Nitrogen dioxide concentration (micrograms per cubic meter)

- co:

  Carbon monoxide concentration (micrograms per cubic meter)

- o3:

  Ozone concentration (micrograms per cubic meter)

- temperature:

  Temperature (degrees Celsius)

- pressure:

  Atmospheric pressure (hPa)

- dew_point:

  Dew point temperature (degrees Celsius)

- rainfall:

  Rainfall (millimeters)

- wind_direction:

  Wind direction

- wind_speed:

  Wind speed (meters per second)

- station:

  Name of the monitoring station

## Source

UCI Machine Learning Repository
<https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data>

## Value

Data frame

## References

Hyndman, R J (2026) *That's weird: Anomaly detection using R*,
<https://OTexts.com/weird/>.

## Examples

``` r
if (FALSE) { # \dontrun{
air_quality <- fetch_air_quality()
air_quality |>
  filter(station == "Aotizhongxin") |>
  ggplot(aes(x = temperature, y = pm2_5)) +
  geom_point(alpha = 0.1)
} # }
```
