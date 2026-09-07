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
#>  [1] 1.0113484 1.2371993 1.6046693 1.0450606 1.8811696 1.2319523 1.4469928
#>  [8] 1.1851282 1.0300407 1.0638319 1.5407789 1.0907294 1.2765567 1.0081514
#> [15] 0.9739824 1.1057016 1.5563971 1.0020825 1.1851282 1.4984793 1.4353401
#> [22] 0.9761441 0.9965181 0.9620022 1.5446967 0.9581303 1.1851282 1.2495157
#> [29] 0.9551818 0.9813890 1.5282988 1.1895354 0.9551818 1.2097969 1.0117110
#> [36] 1.8350833 0.9479384 0.9431167 1.2055177 1.0726014 1.6066256 1.0308146
#> [43] 1.5874197 0.9803188 1.1682119 0.9572218 1.1344011 0.9597531 1.0188441
#> [50] 4.0921711
```
