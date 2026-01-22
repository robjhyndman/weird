# Stray anomalies

Test if observations are anomalies according to the stray algorithm.

## Usage

``` r
stray_anomalies(y, ...)
```

## Arguments

- y:

  A vector, matrix, or data frame consisting of numerical variables.

- ...:

  Other arguments are passed to
  [`find_HDoutliers`](https://rdrr.io/pkg/stray/man/find_HDoutliers.html).

## Value

Numerical vector containing logical values indicating if the observation
is identified as an anomaly using the stray algorithm.

## References

P D Talagala, R J Hyndman and K Smith-Miles (2021) Anomaly detection in
high-dimensional data, *Journal of Computational and Graphical
Statistics*, **30**(2), 360-374.

## Author

Rob J Hyndman

## Examples

``` r
# Univariate data
y <- c(6, rnorm(49))
stray_anomalies(y)
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [49] FALSE FALSE
# Bivariate data
y <- cbind(rnorm(50), c(5, rnorm(49)))
stray_anomalies(y)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [49] FALSE FALSE
```
