# Local outlier factors

Compute local outlier factors using k nearest neighbours. A local
outlier factor is a measure of how anomalous each observation is based
on the density of neighbouring points. The function uses
`dbscan::`[`lof`](http://michael.hahsler.net/dbscan/reference/lof.md) to
do the calculation.

## Usage

``` r
lof_scores(y, k = 10, ...)
```

## Arguments

- y:

  Numerical matrix or vector of data

- k:

  Number of neighbours to include, not counting the observation itself.
  Default: 10.

- ...:

  Additional arguments passed to
  `dbscan::`[`lof`](http://michael.hahsler.net/dbscan/reference/lof.md)

## Value

Numerical vector containing LOF values. An observation has an infinite
LOF when its neighbourhood includes at least `k + 1` identical
observations (whose local reachability density is infinite) but it is
not one of them; the identical observations themselves have LOF values
of 1.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section
6.6, <https://OTexts.com/weird/>.

## See also

`dbscan::`[`lof`](http://michael.hahsler.net/dbscan/reference/lof.md)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
lof_scores(y)
#>  [1] 1.0096678 0.9096734 1.5140288 1.4897710 1.0973761 0.9840656 1.2456017
#>  [8] 1.1178574 1.3149432 1.3044255 0.9338505 1.8223900 1.0121577 1.2950718
#> [15] 0.9700961 0.9859028 0.9778204 2.1336587 1.1054722 1.0201806 1.0360693
#> [22] 0.9612158 1.0527714 1.0015513 1.1031130 1.8841164 1.1770505 1.0253005
#> [29] 0.9398436 1.0620404 1.9280166 1.8653797 1.0860752 0.9639613 1.4615795
#> [36] 1.0138871 1.0428862 1.0060650 1.0428274 2.6696275 1.5135282 0.9262469
#> [43] 0.9945040 0.9922049 1.0053606 1.6288322 1.0193277 0.9906005 1.7039236
#> [50] 5.5399565
```
