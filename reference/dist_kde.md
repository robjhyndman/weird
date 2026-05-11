# Create distributional object based on a kernel density estimate

Creates a distributional object using a kernel density estimate with a
Gaussian kernel obtained from the
[`kde()`](https://mvstat.net/ks/reference/kde.html) function. The
bandwidth can be specified; otherwise the
[`kde_bandwidth()`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
function is used. The cdf, quantiles and moments are consistent with the
kde. Generating random values from the kde is equivalent to a smoothed
bootstrap.

## Usage

``` r
dist_kde(
  y,
  h = NULL,
  H = NULL,
  method = c("robust", "normal", "plugin", "lookout"),
  ...
)
```

## Arguments

- y:

  Numerical vector or matrix of data, or a list of such objects. If a
  list is provided, then all objects should be of the same dimension.
  e.g., all vectors, or all matrices with the same number of columns.

- h:

  Bandwidth for univariate distribution. Ignored if `y` has 2 or more
  columns. If `NULL`, the
  [`kde_bandwidth`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
  function is used.

- H:

  Bandwidth matrix for multivariate distribution. If `NULL`, the
  [`kde_bandwidth`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
  function is used.

- method:

  The method of bandwidth estimation to use. See
  [`kde_bandwidth()`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
  for details. Ignored if `h` or `H` are specified.

- ...:

  Other arguments are passed to
  [`kde`](https://mvstat.net/ks/reference/kde.html).

## Value

A distributional object of class `dist_kde`.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section
2.7 and 3.9, <https://OTexts.com/weird/>.

## Examples

``` r
dist_kde(c(rnorm(200), rnorm(100, 5)))
#> <distribution[1]>
#> [1] kde[1d, h=0.66]
dist_kde(cbind(rnorm(200), rnorm(200, 5)))
#> <distribution[1]>
#> [1] kde[2d, H={(0.19, 0.024)', (0.024, 0.15)'}]
```
