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
#>  [1] 0.07898996 0.02767221 0.82135868 0.61945643 0.78654882 0.17698951
#>  [7] 0.00000000 0.32744488 0.81407287 0.49195760 0.52835678 0.76051843
#> [13] 0.37414898 0.42631136 0.00000000 0.56329515 0.64045302 0.30074952
#> [19] 0.00000000 0.28382172 0.84357149 0.20488727 0.34450321 0.16706532
#> [25] 0.30319016 0.00000000 0.24056271 0.34994927 0.10078685 0.59032652
#> [31] 0.32704110 0.88287840 0.50836143 0.03330645 0.28800509 0.03330645
#> [37] 0.68838195 0.47023221 0.86837634 0.29060925 0.14603473 0.15940317
#> [43] 0.82041561 0.31071301 0.11459574 0.09640851 0.34599082 0.02472565
#> [49] 0.17740966 0.95867389
```
