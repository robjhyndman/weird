# HDR plot

Produces a 1d or 2d box plot of HDR regions. The darker regions contain
observations with higher probability, while the lighter regions contain
points with lower probability. Observations outside the largest HDR are
shown as individual points. Anomalies with leave-one-out surprisal
probabilities less than 0.005 are optionally shown in black.

## Usage

``` r
gg_hdrboxplot(
  data,
  var1,
  var2 = NULL,
  prob = c(0.5, 0.99),
  color = "#0072b2",
  show_points = FALSE,
  show_anomalies = TRUE,
  scatterplot = show_points,
  alpha = NULL,
  jitter = TRUE,
  ngrid = 501,
  ...
)
```

## Arguments

- data:

  A data frame or matrix containing the data.

- var1:

  The name of the first variable to plot (a bare expression).

- var2:

  Optionally, the name of the second variable to plot (a bare
  expression).

- prob:

  A numeric vector specifying the coverage probabilities for the HDRs.

- color:

  The base color to use for the mode. Colors for the HDRs are generated
  by whitening this color.

- show_points:

  A logical argument indicating if a regular HDR plot is required
  (`FALSE`), or whether to show the individual observations in the same
  colors (`TRUE`).

- show_anomalies:

  A logical argument indicating if the surprisal anomalies should be
  shown (in black). These are points with leave-one-out surprisal
  probability values less than 0.005, and which lie outside the 99% HDR
  region.

- scatterplot:

  Equivalent to `show_points`. Included for compatibility with
  [`gg_bagplot()`](https://pkg.robjhyndman.com/weird/reference/bagplot.md).

- alpha:

  Transparency of points. Ignored if `show_points` is `FALSE`. Defaults
  to min(1, 500/n), where n is the number of observations plotted.

- jitter:

  A logical value indicating if the points should be vertically jittered
  for the 1d box plots to reduce overplotting.

- ngrid:

  Number of grid points to use for the density function.

- ...:

  Other arguments passed to
  [`dist_kde`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md).

## Value

A ggplot object showing an HDR plot or scatterplot of the data.

## Details

The original HDR boxplot proposed by Hyndman (1996), can be produced
with `show_anomalies = FALSE`, `jitter = FALSE`, `alpha = 1`, and all
other arguments set to their defaults.

## References

Hyndman, R J (1996) Computing and Graphing Highest Density Regions, *The
American Statistician*, **50**(2), 120–126.
<https://robjhyndman.com/publications/hdr/>

## See also

[`surprisals`](https://pkg.robjhyndman.com/weird/reference/surprisals.md),
[`hdr_table`](https://pkg.robjhyndman.com/weird/reference/hdr_table.md)

## Author

Rob J Hyndman

## Examples

``` r
df <- data.frame(x = c(rnorm(1000), rnorm(1000, 5, 1), 10))
gg_hdrboxplot(df, x, show_anomalies = TRUE)

cricket_batting |>
  filter(Innings > 20) |>
  gg_hdrboxplot(Average)

oldfaithful |>
  gg_hdrboxplot(duration, waiting, show_points = TRUE)

```
