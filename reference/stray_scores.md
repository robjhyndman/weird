# Stray scores

Compute stray scores indicating how anomalous each observation is.

## Usage

``` r
stray_scores(y, ...)
```

## Arguments

- y:

  A vector, matrix, or data frame consisting of numerical variables.

- ...:

  Other arguments are passed to
  [`find_HDoutliers`](https://rdrr.io/pkg/stray/man/find_HDoutliers.html).

## Value

Numerical vector containing stray scores.

## Author

Rob J Hyndman

## Examples

``` r
# Univariate data
y <- c(6, rnorm(49))
scores <- stray_scores(y)
threshold <- stray::find_threshold(scores, alpha = 0.01, outtail = "max", p = 0.5, tn = 50)
which(scores > threshold)
#> integer(0)
```
