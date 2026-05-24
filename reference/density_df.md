# Convert distributional object to a data frame

Make a long-form data frame containing densities from a distributional
object on a regular grid for plotting.

## Usage

``` r
density_df(object, ngrid = NULL)
```

## Arguments

- object:

  A distributional object

- ngrid:

  Number of grid points in each dimension. Defaults to 501 for
  univariate distributions and 101 for bivariate distributions.

## Value

Data frame with columns `x`, `y` (if bivariate), `density`, and
`distribution`.

## Examples

``` r
dist_kde(oldfaithful$duration) |> density_df()
#> # A tibble: 501 × 3
#>        x distribution        density
#>    <dbl> <chr>                 <dbl>
#>  1 -17.9 kde[1d, h=5.1] 0           
#>  2 -17.8 kde[1d, h=5.1] 0.0000000208
#>  3 -17.1 kde[1d, h=5.1] 0.0000000363
#>  4 -16.5 kde[1d, h=5.1] 0.0000000603
#>  5 -15.8 kde[1d, h=5.1] 0.0000000970
#>  6 -15.1 kde[1d, h=5.1] 0.000000151 
#>  7 -14.4 kde[1d, h=5.1] 0.000000230 
#>  8 -13.7 kde[1d, h=5.1] 0.000000355 
#>  9 -13.0 kde[1d, h=5.1] 0.000000535 
#> 10 -12.3 kde[1d, h=5.1] 0.000000786 
#> # ℹ 491 more rows
```
