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

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section
7.3, <https://OTexts.com/weird/>.

## See also

`dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
lof_scores(y)
#>  [1] 1.3186819 1.1539260 1.0058482 0.9615597 1.0519375 1.0365624 0.9837844
#>  [8] 1.2987185 1.6875478 0.9987278 1.0059282 0.9815522 1.2259258 1.0064778
#> [15] 0.9381905 1.0123131 1.0472995 1.2943725 1.0975094 1.5814479 1.1508841
#> [22] 1.2519335 1.0519375 1.0753638 1.0219730 1.2591000 1.0849905 1.0548393
#> [29] 1.0017486 0.9755343 1.1012092 1.2788583 0.9908198 1.0519375 1.5107512
#> [36] 1.0383294 0.9755343 1.0272787 0.9347134 1.2703740 0.9886944 1.0491728
#> [43] 0.9755611 0.9852132 0.9832721 1.2231865 1.0881447 0.9852132 0.9721458
#> [50] 4.3755920
```
