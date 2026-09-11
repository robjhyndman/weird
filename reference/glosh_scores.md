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
#>  [1] 0.00000000 0.01754572 0.76969372 0.93582483 0.84906374 0.67656643
#>  [7] 0.15393696 0.02035276 0.56462834 0.11196128 0.21918053 0.74970353
#> [13] 0.73269489 0.83338823 0.02299376 0.80384078 0.15789928 0.69009032
#> [19] 0.21636370 0.72829334 0.70943679 0.00000000 0.45456518 0.14939790
#> [25] 0.21863590 0.18314533 0.28498159 0.20511082 0.18314533 0.14129317
#> [31] 0.14081447 0.69600594 0.00000000 0.75513455 0.74063588 0.00000000
#> [37] 0.85580373 0.18020154 0.72574932 0.60051605 0.69407526 0.47308734
#> [43] 0.82317024 0.80303601 0.74057542 0.71472745 0.69286469 0.70035739
#> [49] 0.11565288 0.97188460
```
