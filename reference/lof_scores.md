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
6.6, <https://OTexts.com/weird/>.

## See also

`dbscan::`[`lof`](https://rdrr.io/pkg/dbscan/man/lof.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
lof_scores(y)
#>  [1] 0.9983730 0.9530704 1.4581287 1.4350465 1.0936686 0.9831049 1.2510033
#>  [8] 1.1366891 1.2742153 1.2479500 0.9876898 1.7937903 0.9711415 1.2997004
#> [15] 0.9824765 0.9685810 0.9920072 1.9850663 1.1054990 0.9834240 1.0711687
#> [22] 0.9715196 1.0551016 0.9955457 1.0936686 1.7614975 1.1938283 1.0106213
#> [29] 0.9925144 1.0547248 1.8030587 1.8323102 1.0517139 0.9638423 1.3947665
#> [36] 1.0356019 1.0517139 0.9634494 1.0517139 2.7723964 1.4698172 0.9783248
#> [43] 0.9948258 0.9538828 1.0162370 1.5496747 0.9801937 0.9585490 1.6119820
#> [50] 5.2982752
```
