# GLOSH scores

Compute Global-Local Outlier Score from Hierarchies. This is based on
hierarchical clustering where the minimum cluster size is k. The
resulting outlier score is a measure of how anomalous each observation
is. The function uses
`dbscan::`[`hdbscan`](https://rdrr.io/pkg/dbscan/man/hdbscan.html) to do
the calculation.

## Usage

``` r
glosh_scores(y, k = 10, ...)
```

## Arguments

- y:

  Numerical matrix or vector of data

- k:

  Minimum cluster size. Default: 5.

- ...:

  Additional arguments passed to
  `dbscan::`[`hdbscan`](https://rdrr.io/pkg/dbscan/man/hdbscan.html)

## Value

Numerical vector containing GLOSH values

## See also

`dbscan::`[`glosh`](https://rdrr.io/pkg/dbscan/man/glosh.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
glosh_scores(y)
#>  [1] 0.12699448 0.64273013 0.79844228 0.43444519 0.00000000 0.23366298
#>  [7] 0.46290261 0.25321690 0.14141241 0.20984452 0.14187241 0.00000000
#> [13] 0.45795231 0.19232766 0.26695668 0.82081973 0.45947376 0.09156258
#> [19] 0.29104784 0.28875389 0.68625231 0.09156258 0.84897011 0.23550826
#> [25] 0.25008251 0.17631255 0.22665993 0.07367014 0.23550826 0.72740468
#> [31] 0.10125935 0.17173028 0.35438028 0.09667300 0.17984478 0.61503752
#> [37] 0.64249689 0.14865598 0.36906599 0.17173028 0.36206306 0.19048791
#> [43] 0.23366298 0.00000000 0.65192809 0.40074599 0.00000000 0.21549002
#> [49] 0.18766398 0.94880290
```
