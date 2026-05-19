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

- gun_ownership_rate:

  Gun ownership rate (number of guns owned by civilians per 100 people)

- homicide_rate:

  Homicide rate (number of homicides per 100,000 people where the weapon
  was a firearm)

- region:

  World region according to Our World in Data

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
#>    country                gun_ownership_rate homicide_rate region       
#>    <chr>                               <dbl>         <dbl> <chr>        
#>  1 United States                       120.          3.32  North America
#>  2 Serbia                               39.1         0.438 Europe       
#>  3 Montenegro                           39.1         1.77  Europe       
#>  4 Canada                               34.7         0.725 North America
#>  5 Uruguay                              34.7         5.02  South America
#>  6 Cyprus                               34           0.159 Europe       
#>  7 Finland                              32.4         0.109 Europe       
#>  8 Iceland                              31.7         0     Europe       
#>  9 Bosnia and Herzegovina               31.2         0.671 Europe       
#> 10 Austria                              30           0.182 Europe       
#> # ℹ 67 more rows
```
