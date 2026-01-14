# Create distributional object based on a kernel density estimate

Creates a distributional object using a kernel density estimate with a
Gaussian kernel obtained from the
[`kde()`](https://rdrr.io/pkg/ks/man/kde.html) function. The bandwidth
can be specified; otherwise the
[`kde_bandwidth()`](https://pkg.robjhyndman.com/weird-package/reference/kde_bandwidth.md)
function is used. The cdf, quantiles and moments are consistent with the
kde. Generating random values from the kde is equivalent to a smoothed
bootstrap.

## Usage

``` r
dist_kde(y, h = NULL, H = NULL, lookout = FALSE, multiplier = 1, ...)
```

## Arguments

- y:

  Numerical vector or matrix of data, or a list of such objects. If a
  list is provided, then all objects should be of the same dimension.
  e.g., all vectors, or all matrices with the same number of columns.

- h:

  Bandwidth for univariate distribution. If `NULL`, the
  [`kde_bandwidth`](https://pkg.robjhyndman.com/weird-package/reference/kde_bandwidth.md)
  function is used.

- H:

  Bandwidth matrix for multivariate distribution. If `NULL`, the
  [`kde_bandwidth`](https://pkg.robjhyndman.com/weird-package/reference/kde_bandwidth.md)
  function is used.

- lookout:

  A logical variable passed to
  [`kde_bandwidth()`](https://pkg.robjhyndman.com/weird-package/reference/kde_bandwidth.md)
  (set to
  ``` FALSE`` by default) indicating which bandwidth estimator to use. Ignored if  ```h`or`H\`
  are specified.

- multiplier:

  Multiplier for bandwidth passed to
  [`kde_bandwidth`](https://pkg.robjhyndman.com/weird-package/reference/kde_bandwidth.md).
  Ignored if `h` or `H` are specified.

- ...:

  Other arguments are passed to
  [`kde`](https://rdrr.io/pkg/ks/man/kde.html).

## Examples

``` r
dist_kde(c(rnorm(200), rnorm(100, 5)), multiplier = 2)
#> <distribution[1]>
#> [1] kde[1d, h=1.3]
dist_kde(cbind(rnorm(200), rnorm(200, 5)))
#> <distribution[1]>
#> [1] kde[2d, H={(0.19, 0.022)', (0.022, 0.16)'}]
```
