# French mortality rates by age and sex

A data set containing French mortality rates between the years 1816 and
1999, by age and sex.

## Usage

``` r
fr_mortality
```

## Format

A data frame with 31,648 rows and 4 columns.

## Source

Human Mortality Database <https://www.mortality.org>

## Value

Data frame

## References

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section
1.4, <https://otexts.com/weird/>.

## Examples

``` r
fr_mortality
#> # A tibble: 31,648 × 4
#>     Year   Age Sex    Mortality
#>    <int> <int> <chr>      <dbl>
#>  1  1816     0 Female   0.187  
#>  2  1816     1 Female   0.0467 
#>  3  1816     2 Female   0.0339 
#>  4  1816     3 Female   0.0229 
#>  5  1816     4 Female   0.0160 
#>  6  1816     5 Female   0.0138 
#>  7  1816     6 Female   0.0121 
#>  8  1816     7 Female   0.0104 
#>  9  1816     8 Female   0.00891
#> 10  1816     9 Female   0.00760
#> # ℹ 31,638 more rows
```
