# Robust bandwidth estimation for kernel density estimation

By default, bandwidths are chosen using a robust version of the normal
reference rule. Alternatively, they can be estimated using the approach
of Hyndman, Kandanaarachchi & Turner (2026) if `lookout = TRUE`. The
resulting bandwidth is scaled by `multiplier` (set to 1 by default).

## Usage

``` r
kde_bandwidth(data, lookout = FALSE, multiplier = 1, ...)
```

## Arguments

- data:

  A numeric matrix or data frame.

- lookout:

  A logical variable (set to \`FALSE“ by default) indicating that the
  bandwidth matrix estimate of Hyndman, Kandanaarachchi & Turner (2026)
  is returned.

- multiplier:

  Bandwidth scaling factor (squared if the data dimension is greater
  than 1).

- ...:

  Additional arguments are ignored if `lookout = FALSE` and passed to
  [`lookout::find_tda_bw()`](https://sevvandi.github.io/lookout/reference/find_tda_bw.html)
  otherwise.

## Value

A matrix of bandwidths (or a scalar in the case of univariate data).

## References

Rob J Hyndman, Sevvandi Kandanaarachchi & Katharine Turner (2026) "When
lookout sees crackle: Anomaly detection via kernel density estimation",
unpublished. <https://robjhyndman.com/publications/lookout2/>

Rob J Hyndman (2026) "That's weird: Anomaly detection using R",
<https://OTexts.com/weird/>

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
