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
#>  [1] 1.0151701 0.9795930 1.3624451 1.0525696 0.9820816 1.0114416 1.1664035
#>  [8] 1.6891939 1.0357432 1.9448950 1.3017274 1.3568244 1.2240889 1.0471630
#> [15] 1.0717023 1.5936164 1.0805781 1.2040674 0.9696911 0.9795930 1.1000173
#> [22] 1.6300830 0.9696911 1.2125533 1.5458695 1.2721561 0.9795930 1.0058951
#> [29] 0.9574805 1.5976595 0.9514840 1.1810930 1.0441963 0.9568731 0.9795930
#> [36] 1.5807371 1.2448507 0.9557738 1.0467010 1.0118338 1.4381934 0.9334593
#> [43] 0.9438781 1.0025755 0.9752217 1.6912465 1.0050113 1.6417491 0.9704712
#> [50] 4.1416137
```
