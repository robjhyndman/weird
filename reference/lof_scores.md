# Local outlier factors

Compute local outlier factors using k nearest neighbours. A local
outlier factor is a measure of how anomalous each observation is based
on the density of neighbouring points. The function uses
`dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html) to do the
calculation.

## Usage

``` r
lof_scores(y, k = 10, ...)
```

## Arguments

- y:

  Numerical matrix or vector of data

- k:

  Number of neighbours to include. Default: 5.

- ...:

  Additional arguments passed to
  `dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html)

## Value

Numerical vector containing LOF values

## See also

`dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
lof_scores(y)
#>  [1] 1.1264240 1.0060939 1.0431067 0.9531149 1.1772442 0.9762368 1.0132530
#>  [8] 1.0408435 2.6439207 1.1069737 0.9660165 0.9739231 1.0376997 0.9648683
#> [15] 1.0006166 0.9838663 2.1874189 1.0477058 0.9759503 1.6809160 0.9846755
#> [22] 1.4697662 0.9837292 0.9551825 1.5628546 4.1397725 1.0107599 1.0330016
#> [29] 2.4715562 0.9200622 1.8078043 1.0063005 0.9531149 0.9768929 1.4317919
#> [36] 1.3978906 1.1080808 1.5061760 1.3447905 1.0403518 0.9862803 1.0130588
#> [43] 0.9654478 0.9651093 1.0334289 1.7647009 2.3175428 1.0460067 0.9316119
#> [50] 9.2830362
```
