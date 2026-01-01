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
#>  [1] 0.22998175 0.13770592 0.00000000 0.50216476 0.10085345 0.21233471
#>  [7] 0.81638234 0.50356211 0.24674124 0.34887395 0.40713836 0.63090402
#> [13] 0.10978182 0.84799465 0.29786450 0.37802907 0.00000000 0.37802907
#> [19] 0.09727780 0.21233471 0.70270996 0.12416389 0.23928860 0.46682546
#> [25] 0.11969442 0.37857913 0.47562942 0.67413352 0.28004749 0.41986011
#> [31] 0.32035853 0.41341344 0.00000000 0.36575737 0.17059295 0.54685774
#> [37] 0.44797990 0.00000000 0.27947906 0.16374192 0.71062970 0.46998476
#> [43] 0.05709659 0.27085566 0.10085345 0.83845358 0.17059295 0.44797990
#> [49] 0.33586203 0.95101224
```
