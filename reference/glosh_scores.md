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
#>  [1] 0.542969464 0.344956031 0.070042530 0.394834135 0.070746994 0.152549277
#>  [7] 0.538757127 0.293350776 0.212334708 0.841861689 0.540051767 0.246741238
#> [13] 0.348873955 0.362265250 0.682920065 0.109781817 0.867480345 0.297864499
#> [19] 0.328457051 0.000000000 0.378029068 0.290540631 0.212334708 0.754661047
#> [25] 0.007322072 0.239288605 0.466303177 0.000000000 0.378579130 0.562909724
#> [31] 0.698085668 0.280047486 0.419860105 0.320358534 0.413413439 0.000000000
#> [37] 0.314128724 0.270855665 0.617064044 0.449624509 0.000000000 0.279479056
#> [43] 0.342775519 0.737952978 0.560380052 0.057096587 0.270855665 0.293350776
#> [49] 0.850327719 0.955561865
```
