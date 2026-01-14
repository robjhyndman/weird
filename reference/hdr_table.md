# Table of Highest Density Regions

Compute a table of highest density regions (HDR) for a distributional
object. The HDRs are returned as a tibble with one row per interval and
columns: `prob` (giving the probability coverage), `density` (the value
of the density at the boundary of the HDR), For one dimensional density
functions, the tibble also has columns `lower` (the lower ends of the
intervals), and `upper` (the upper ends of the intervals).

## Usage

``` r
hdr_table(object, prob)
```

## Arguments

- object:

  Distributional object such as that returned by
  [`dist_kde()`](https://pkg.robjhyndman.com/weird-package/reference/dist_kde.md)

- prob:

  Vector of probabilities giving the HDR coverage (between 0 and 1)

## Value

A tibble

## Author

Rob J Hyndman

## Examples

``` r
# Univariate HDRs
c(dist_normal(), dist_kde(c(rnorm(100), rnorm(100, 3, 1)))) |>
  hdr_table(c(0.5, 0.95))
#> # A tibble: 5 × 5
#>    prob distribution     lower upper density
#>   <dbl> <chr>            <dbl> <dbl>   <dbl>
#> 1  0.5  N(0, 1)         -0.674 0.674  0.212 
#> 2  0.95 N(0, 1)         -1.96  1.96   0.0516
#> 3  0.5  kde[1d, h=0.66] -0.662 1.00   0.212 
#> 4  0.5  kde[1d, h=0.66]  1.98  3.20   0.212 
#> 5  0.95 kde[1d, h=0.66] -1.75  5.08   0.0516
dist_kde(oldfaithful$duration) |> hdr_table(0.95)
#> # A tibble: 2 × 5
#>    prob distribution   lower upper density
#>   <dbl> <chr>          <dbl> <dbl>   <dbl>
#> 1  0.95 kde[1d, h=5.1]  99.1  136. 0.00106
#> 2  0.95 kde[1d, h=5.1] 185.   290. 0.00106
# Bivariate HDRs
dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_table(0.90)
#> # A tibble: 1 × 3
#>   distribution                                      prob    density
#>   <chr>                                            <dbl>      <dbl>
#> 1 kde[2d, H={(41, 3.5e+02)', (3.5e+02, 2.3e+04)'}]   0.9 0.00000112
```
