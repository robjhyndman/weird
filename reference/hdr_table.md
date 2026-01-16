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
  [`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)

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
#> 2  0.95 N(0, 1)         -1.96  1.96   0.0517
#> 3  0.5  kde[1d, h=0.65] -0.667 0.984  0.212 
#> 4  0.5  kde[1d, h=0.65]  1.99  3.21   0.212 
#> 5  0.95 kde[1d, h=0.65] -1.74  5.07   0.0517
dist_kde(oldfaithful$duration) |> hdr_table(0.95)
#> # A tibble: 2 × 5
#>    prob distribution  lower upper density
#>   <dbl> <chr>         <dbl> <dbl>   <dbl>
#> 1  0.95 kde[1d, h=11]  94.5  140. 0.00108
#> 2  0.95 kde[1d, h=11] 184.   294. 0.00108
# Bivariate HDRs
dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_table(0.90)
#> # A tibble: 1 × 3
#>   distribution                                           prob    density
#>   <chr>                                                 <dbl>      <dbl>
#> 1 kde[2d, H={(1.7e+02, 2.1e+03)', (2.1e+03, 4.1e+04)'}]   0.9 0.00000113
```
