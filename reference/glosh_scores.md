# GLOSH scores

Compute Global-Local Outlier Score from Hierarchies. This is based on
hierarchical clustering, using core distances to the k-th nearest
neighbour. The resulting outlier score is a measure of how anomalous
each observation is. The function uses
`dbscan::`[`hdbscan`](http://michael.hahsler.net/dbscan/reference/hdbscan.md)
to do the calculation.

## Usage

``` r
glosh_scores(y, k = 10, ...)
```

## Arguments

- y:

  Numerical matrix or vector of data

- k:

  Number of neighbours to include, not counting the observation itself.
  Default: 10.

- ...:

  Additional arguments passed to
  `dbscan::`[`hdbscan`](http://michael.hahsler.net/dbscan/reference/hdbscan.md)

## Value

Numerical vector containing GLOSH values

## See also

`dbscan::`[`glosh`](http://michael.hahsler.net/dbscan/reference/glosh.md)

## Author

Rob J Hyndman

## Examples

``` r
y <- c(rnorm(49), 5)
glosh_scores(y)
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [39] 1 1 1 1 1 1 1 1 1 1 1 1
```
