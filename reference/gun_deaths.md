# Gun ownership and homicide rates by country

A data set containing gun ownership rates and homicide rates for 2017
for various countries around the world. The gun ownership rates are the
number of guns owned by civilians per 100 people. The homicide rates are
the number of homicides per 100,000 people where the weapon was a
firearm.

## Usage

``` r
gun_deaths
```

## Format

A data frame with 77 rows and 4 columns:

- country:

  Country name

- region:

  World region according to Our World in Data

- gun_ownership_rate:

  Gun ownership rate (number of guns owned by civilians per 100 people)

- homicide_rate:

  Homicide rate (number of homicides per 100,000 people where the weapon
  was a firearm)

## Source

World Population Review
<https://worldpopulationreview.com/country-rankings/gun-ownership-by-country>
and <https://ourworldindata.org/grapher/homicide-rates-from-firearms>

## Value

Data frame

## Examples

``` r
gun_deaths
#> # A tibble: 77 × 4
#>    country             region        gun_ownership_rate homicide_rate
#>    <chr>               <chr>                      <dbl>         <dbl>
#>  1 Albania             Europe                      12          1.52  
#>  2 Algeria             Africa                       2.1        0.127 
#>  3 Antigua and Barbuda North America                5.4       11.1   
#>  4 Argentina           South America                7.4        2.81  
#>  5 Armenia             Asia                         6.1        0.618 
#>  6 Australia           Oceania                     14.5        0.0930
#>  7 Austria             Europe                      30          0.182 
#>  8 Azerbaijan          Asia                         3.6        0.181 
#>  9 Bahamas             North America               18.8       27.4   
#> 10 Barbados            North America                3.5        8.21  
#> # ℹ 67 more rows
```
