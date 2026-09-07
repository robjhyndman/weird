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

## References

Hyndman, R J (1996) "Computing and Graphing Highest Density Regions",
*The American Statistician*, 50(2), 120–126.
<https://robjhyndman.com/publications/hdr/>

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section
2.7, 3.4. <https://OTexts.com/weird/>.

## See also

[`gg_hdrboxplot`](https://pkg.robjhyndman.com/weird/reference/gg_hdrboxplot.md)

## Author

Rob J Hyndman

## Examples

``` r
# Univariate HDRs
c(dist_normal(), dist_kde(c(rnorm(100), rnorm(100, 3, 1)))) |>
  hdr_table(c(0.5, 0.95))
#> # A tibble: 5 × 5
#>   distribution    prob  lower upper density
#>   <chr>          <dbl>  <dbl> <dbl>   <dbl>
#> 1 N(0, 1)         0.5  -0.674 0.674  0.318 
#> 2 N(0, 1)         0.95 -1.96  1.96   0.0584
#> 3 kde[1d, h=0.7]  0.5  -0.652 0.648  0.158 
#> 4 kde[1d, h=0.7]  0.5   1.91  3.16   0.158 
#> 5 kde[1d, h=0.7]  0.95 -1.63  4.66   0.0721
dist_kde(oldfaithful$duration) |> hdr_table(0.95)
#> # A tibble: 2 × 5
#>   distribution    prob lower upper density
#>   <chr>          <dbl> <dbl> <dbl>   <dbl>
#> 1 kde[1d, h=5.1]  0.95  99.7  135. 0.00117
#> 2 kde[1d, h=5.1]  0.95 190    289. 0.00117
# Bivariate HDRs
dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_table(0.90)
#> # A tibble: 1 × 3
#>   distribution                                      prob    density
#>   <chr>                                            <dbl>      <dbl>
#> 1 kde[2d, H={(41, 3.5e+02)', (3.5e+02, 2.3e+04)'}]   0.9 0.00000109
```
