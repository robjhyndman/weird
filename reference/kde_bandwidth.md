# Robust bandwidth estimation for kernel density estimation

Robust bandwidth estimation for kernel density estimation

## Usage

``` r
kde_bandwidth(data, multiplier = 1)
```

## Arguments

- data:

  A numeric matrix or data frame.

- multiplier:

  Bandwidths are chosen using a robust version of the normal reference
  rule multiplied by a constant. The default is 1.

## Value

A matrix of bandwidths (or scalar in the case of univariate data).

## Author

Rob J Hyndman

## Examples

``` r
# Univariate bandwidth calculation
kde_bandwidth(oldfaithful$duration)
#> [1] 5.087699
# Bivariate bandwidth calculation
kde_bandwidth(oldfaithful[, c("duration", "waiting")])
#>           [,1]       [,2]
#> [1,]  40.53793   349.3449
#> [2,] 349.34486 22918.0734
```
