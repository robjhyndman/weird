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

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section
7.3, <https://OTexts.com/weird/>.

## See also

`dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
lof_scores(y)
#>  [1] 1.0333363 0.9565327 1.1071109 1.0072759 0.9956098 1.0520332 2.6439207
#>  [8] 1.0954124 0.9674015 0.9828426 1.0379882 0.9746189 0.9853988 0.9685060
#> [15] 2.2722769 1.0384843 0.9846837 1.6430541 0.9398681 1.4363654 1.0005526
#> [22] 0.9565327 1.4826308 4.2536171 1.0122068 1.0439167 2.4715562 0.9085510
#> [29] 1.7672615 0.9918311 0.9565327 0.9855398 1.4968814 1.3110825 1.0251741
#> [36] 1.4720060 1.2492783 1.0407458 0.9801776 1.0123674 0.9751452 0.9664913
#> [43] 1.0242448 1.7647009 2.4058051 1.0352306 0.9205577 1.0360369 1.1492325
#> [50] 9.4771993
```
