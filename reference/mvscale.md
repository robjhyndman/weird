# Compute robust multivariate scaled data

A multivariate version of
[`base::scale()`](https://rdrr.io/r/base/scale.html), that takes account
of the covariance matrix of the data, and uses robust estimates of
center, scale and covariance by default. The centers are removed using
medians, the scale function is the IQR, and the covariance matrix is
estimated using a robust OGK estimate. The data are scaled using the
Cholesky decomposition of the inverse covariance. Then the scaled data
are returned.

## Usage

``` r
mvscale(
  object,
  center = stats::median,
  scale = robustbase::s_Qn,
  cov = robustbase::covOGK,
  warning = TRUE
)
```

## Arguments

- object:

  A vector, matrix, or data frame containing some numerical data.

- center:

  A function to compute the center of each numerical variable. Set to
  NULL if no centering is required.

- scale:

  A function to scale each numerical variable. When
  `cov = robustbase::covOGK()`, it is passed as the `sigmamu` argument.

- cov:

  A function to compute the covariance matrix. Set to NULL if no
  rotation required.

- warning:

  Should a warning be issued if non-numeric columns are ignored?

## Value

A vector, matrix or data frame of the same size and class as `object`,
but with numerical variables replaced by scaled versions.

## Details

Optionally, the centering and scaling can be done for each variable
separately, so there is no rotation of the data, by setting
`cov = NULL`. Also optionally, non-robust methods can be used by
specifying `center = mean`, `scale = stats::sd()`, and
`cov = stats::cov()`. Any non-numeric columns are retained with a
warning.

## See also

[`base::scale()`](https://rdrr.io/r/base/scale.html),
[`stats::sd()`](https://rdrr.io/r/stats/sd.html),
[`stats::cov()`](https://rdrr.io/r/stats/cor.html),
[`robustbase::covOGK()`](https://rdrr.io/pkg/robustbase/man/covOGK.html),
[`robustbase::s_Qn()`](https://rdrr.io/pkg/robustbase/man/Qn.html)

## Author

Rob J Hyndman

## Examples

``` r
# Univariate z-scores (no rotation)
z <- mvscale(faithful, center = mean, scale = sd, cov = NULL, warning = FALSE)
# Non-robust scaling with rotation
z <- mvscale(faithful, center = mean, cov = stats::cov, warning = FALSE)
# Robust scaling and rotation
z <- mvscale(faithful, warning = FALSE)
```
