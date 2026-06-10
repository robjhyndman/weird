# Compute robust multivariate scaled data

A multivariate version of
[`base::scale()`](https://rdrr.io/r/base/scale.html), that takes account
of the covariance matrix of the data. By default, robust estimates are
used: the centers are removed using medians, the scale function for
univariate data is `s_Qn`, and the covariance matrix for multivariate
data is estimated using a robust MCD estimate. The data are scaled using
the Cholesky decomposition of the inverse (co)variance. Then the scaled
data are returned. Details of the methods are provided by Hyndman
(2026).

## Usage

``` r
mvscale(
  object,
  center = stats::median,
  scale = robustbase::s_Qn,
  cov = robustbase::covMcd,
  alpha = 0.9,
  warning = TRUE,
  ...
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
  `cov = robustbase::covOGK()`, `scale` is passed as the `sigmamu`
  argument. When `cov = robustbase::covMcd()`, `scale` is passed as the
  `scalefn` argument.

- cov:

  A function to compute the covariance matrix. Set to NULL if no
  rotation required. [`cov()`](https://rdrr.io/r/stats/cor.html) must
  either return the matrix directly, or a list containing a matrix named
  `cov`.

- alpha:

  When `cov = robustbase::covMcd()`, `alpha` controls the size of the
  subsets over which the determinant is minimized. Otherwise it is
  ignored. Set to 0.9 by default.

- warning:

  Should a warning be issued if non-numeric columns are ignored?

- ...:

  Other arguments are passed to
  [`cov()`](https://rdrr.io/r/stats/cor.html).

## Value

A vector, matrix or data frame of the same size and class as `object`,
but with numerical variables replaced by scaled versions (renamed if
they have been rotated).

## Details

Optionally, the centering and scaling can be done for each variable
separately, by setting `cov = NULL`, so there is no rotation of the
data, Also optionally, non-robust methods can be used by specifying
`center = mean`, `scale = stats::sd()`, and `cov = stats::cov()`. Any
non-numeric columns are retained with a warning. Missing values are
removed before the centers, scale and cov are estimated.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Section
2.6, 3.6 and 3.7.

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
# Univariate z-scores
z <- mvscale(oldfaithful$duration, center = mean, scale = sd)
# Non-robust scaling with no rotation
oldfaithful |>
  mvscale(center = mean, scale = sd, cov = NULL, warning = FALSE)
#> # A tibble: 2,097 × 4
#>    time                recorded_duration duration waiting
#>    <dttm>              <chr>                <dbl>   <dbl>
#>  1 2017-01-14 00:06:00 3m 16s              -0.651  0.455 
#>  2 2017-01-26 14:27:00 ~4m                  0.291  0.290 
#>  3 2017-01-27 23:57:00 2m 1s               -2.26  -2.35  
#>  4 2017-01-30 15:09:00 ~4m                  0.291 -0.453 
#>  5 2017-01-31 13:27:00 ~3.5m               -0.352 -0.0404
#>  6 2017-01-31 15:00:00 ~4m                  0.291  0.207 
#>  7 2017-02-03 23:13:00 3m 25s              -0.459 -0.618 
#>  8 2017-02-04 22:14:00 3m 34s              -0.266 -0.288 
#>  9 2017-02-05 17:19:00 4m 0s                0.291  0.620 
#> 10 2017-02-05 19:00:00 4m 2s                0.334  0.620 
#> # ℹ 2,087 more rows
# Non-robust scaling with rotation
oldfaithful |>
  mvscale(center = mean, scale = sd, cov = stats::cov, warning = FALSE)
#> # A tibble: 2,097 × 4
#>    time                recorded_duration      z1      z2
#>    <dttm>              <chr>               <dbl>   <dbl>
#>  1 2017-01-14 00:06:00 3m 16s            -1.65    0.455 
#>  2 2017-01-26 14:27:00 ~4m                0.101   0.290 
#>  3 2017-01-27 23:57:00 2m 1s             -0.653  -2.35  
#>  4 2017-01-30 15:09:00 ~4m                1.06   -0.453 
#>  5 2017-01-31 13:27:00 ~3.5m             -0.521  -0.0404
#>  6 2017-01-31 15:00:00 ~4m                0.207   0.207 
#>  7 2017-02-03 23:13:00 3m 25s             0.0483 -0.618 
#>  8 2017-02-04 22:14:00 3m 34s            -0.0627 -0.288 
#>  9 2017-02-05 17:19:00 4m 0s             -0.324   0.620 
#> 10 2017-02-05 19:00:00 4m 2s             -0.254   0.620 
#> # ℹ 2,087 more rows
# Robust scaling and rotation
oldfaithful |>
  mvscale(warning = FALSE)
#> # A tibble: 2,097 × 4
#>    time                recorded_duration      z1     z2
#>    <dttm>              <chr>               <dbl>  <dbl>
#>  1 2017-01-14 00:06:00 3m 16s            -2.34    0.394
#>  2 2017-01-26 14:27:00 ~4m               -0.0524  0.131
#>  3 2017-01-27 23:57:00 2m 1s             -4.28   -4.07 
#>  4 2017-01-30 15:09:00 ~4m                0.419  -1.05 
#>  5 2017-01-31 13:27:00 ~3.5m             -1.33   -0.394
#>  6 2017-01-31 15:00:00 ~4m                0       0    
#>  7 2017-02-03 23:13:00 3m 25s            -1.21   -1.31 
#>  8 2017-02-04 22:14:00 3m 34s            -0.976  -0.787
#>  9 2017-02-05 17:19:00 4m 0s             -0.262   0.656
#> 10 2017-02-05 19:00:00 4m 2s             -0.163   0.656
#> # ℹ 2,087 more rows
```
