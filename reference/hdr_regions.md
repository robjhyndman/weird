# Highest density regions for each observation

For a `dist_kde` object, determine which highest density region (HDR)
each observation falls in, for one or more coverage probabilities. Some
densities are multimodal, so the HDR for a given `prob` can consist of
several disjoint regions; the returned columns identify the specific
region (`1`, `2`, ...) that each observation falls in, ordered from
lowest to highest along the first variable, with `NA` for observations
outside the HDR at that `prob`.

## Usage

``` r
hdr_regions(object, prob)
```

## Arguments

- object:

  A `dist_kde` object, as returned by
  [`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md),
  containing a single distribution estimated from univariate or
  bivariate data.

- prob:

  A numeric vector of probabilities giving the HDR coverage (between 0
  and 1).

## Value

A tibble containing the data used to estimate `object`, along with one
additional integer column per element of `prob` (named `hdr_<100*prob>`)
showing which region of the corresponding HDR each observation falls in.

## See also

[`hdr_table`](https://pkg.robjhyndman.com/weird/reference/hdr_table.md),
[`gg_hdrboxplot`](https://pkg.robjhyndman.com/weird/reference/gg_hdrboxplot.md)

## Author

Rob J Hyndman

## Examples

``` r
dist_kde(oldfaithful$duration) |> hdr_regions(c(0.5, 0.95))
#> # A tibble: 2,097 × 3
#>        x hdr_50 hdr_95
#>    <dbl>  <int>  <int>
#>  1   196     NA      2
#>  2   240      1      2
#>  3   121     NA      1
#>  4   240      1      2
#>  5   210     NA      2
#>  6   240      1      2
#>  7   205     NA      2
#>  8   214     NA      2
#>  9   240      1      2
#> 10   242      1      2
#> # ℹ 2,087 more rows
dist_kde(oldfaithful[, c("duration", "waiting")]) |> hdr_regions(0.90)
#> # A tibble: 2,097 × 3
#>    duration waiting hdr_90
#>       <dbl>   <dbl>  <int>
#>  1      196    5940     NA
#>  2      240    5820      2
#>  3      121    3900      1
#>  4      240    5280      2
#>  5      210    5580      2
#>  6      240    5760      2
#>  7      205    5160      2
#>  8      214    5400      2
#>  9      240    6060      2
#> 10      242    6060      2
#> # ℹ 2,087 more rows
```
