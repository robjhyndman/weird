# Robust bandwidth estimation for kernel density estimation

Bandwidth matrices are estimated using either a robust version of the
normal reference rule, or using the approach of Hyndman, Kandanaarachchi
& Turner (2026).

## Usage

``` r
kde_bandwidth(data, method = c("robust", "normal", "plugin", "lookout"), ...)
```

## Arguments

- data:

  A numeric matrix or data frame.

- method:

  A character string giving the method to use. Possibilities are:
  `"normal"` (normal reference rule), `"robust"` (a robust version of
  the normal reference rule, the default), `"plugin"` (a plugin
  estimator), and `"lookout"` (the bandwidth matrix estimate of Hyndman,
  Kandanaarachchi & Turner, 2026).

- ...:

  Additional arguments are ignored unless `method = "lookout"`, when
  they are passed to
  [`lookout::find_tda_bw()`](https://sevvandi.github.io/lookout/reference/find_tda_bw.html).

## Value

A matrix of bandwidths (or a scalar in the case of univariate data).

## References

Rob J Hyndman, Sevvandi Kandanaarachchi & Katharine Turner (2026) "When
lookout sees crackle: Anomaly detection via kernel density estimation",
unpublished. <https://robjhyndman.com/publications/lookout2.html>

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Section
2.7 and 3.9, <https://OTexts.com/weird/>.

## Author

Rob J Hyndman

## Examples

``` r
# Univariate bandwidth calculation
kde_bandwidth(oldfaithful$duration)
#> [1] 5.087698
# Bivariate bandwidth calculation
kde_bandwidth(oldfaithful[, c("duration", "waiting")])
#>           [,1]       [,2]
#> [1,]  40.53793   349.3449
#> [2,] 349.34486 22918.0734
```
