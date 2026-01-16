# weird ![](reference/figures/weird-hex.png)

## Overview

The weird package contains functions and data used in the book [*That’s
Weird: Anomaly Detection Using R*](https://OTexts.com/weird/) by Rob J
Hyndman. It also loads several packages needed to do the analysis
described in the book.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=weird) with:

``` r
install.packages("weird")
```

You can install the **development** version of weird from
[GitHub](https://github.com/robjhyndman/weird) with:

``` r
# install.packages("pak")
pak::pak("robjhyndman/weird")
```

## Usage

[`library(weird)`](https://pkg.robjhyndman.com/weird/) will load the
following packages:

- [dplyr](https://dplyr.tidyverse.org), for data manipulation.
- [ggplot2](https://ggplot2.tidyverse.org), for data visualisation.
- [distributional](https://cran.r-project.org/package=distributional),
  for handling probability distributions.

You also get a condensed summary of conflicts with other packages you
have loaded:

``` r
library(weird)
#> ── Attaching packages ────────────────────────────────────────────────────────── weird 1.0.2.9000 ──
#> ✔ dplyr          1.1.4     ✔ distributional 0.5.0
#> ✔ ggplot2        4.0.1
#> ── Conflicts ──────────────────────────────────────────────────────────────────── weird_conflicts ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

## Example: Old Faithful Geyser data

The `oldfaithful` data set contains eruption data from the Old Faithful
Geyser in Yellowstone National Park, Wyoming, USA, from 1 January 2015
to 1 October 2021. The data were obtained from the
[geysertimes.org](https://geysertimes.org) website. Recordings are
incomplete, especially during the winter months when observers may not
be present. There also appear to be some recording errors. The data set
contains 2097 observations of 3 variables: `time` giving the time at
which each eruption began, `duration` giving the length of the eruption
in seconds, and `waiting` giving the time to the next eruption in
seconds. In the analysis below, we omit the eruption with `duration`
greater than 1 hour as this is likely to be a recording error. Some of
the long `waiting` values are probably due to omitted eruptions, and so
we also omit eruptions with `waiting` greater than 2 hours.

``` r
oldfaithful
#> # A tibble: 2,097 × 4
#>    time                recorded_duration duration waiting
#>    <dttm>              <chr>                <dbl>   <dbl>
#>  1 2017-01-14 00:06:00 3m 16s                 196    5940
#>  2 2017-01-26 14:27:00 ~4m                    240    5820
#>  3 2017-01-27 23:57:00 2m 1s                  121    3900
#>  4 2017-01-30 15:09:00 ~4m                    240    5280
#>  5 2017-01-31 13:27:00 ~3.5m                  210    5580
#>  6 2017-01-31 15:00:00 ~4m                    240    5760
#>  7 2017-02-03 23:13:00 3m 25s                 205    5160
#>  8 2017-02-04 22:14:00 3m 34s                 214    5400
#>  9 2017-02-05 17:19:00 4m 0s                  240    6060
#> 10 2017-02-05 19:00:00 4m 2s                  242    6060
#> # ℹ 2,087 more rows
```

## Kernel density estimates

The package provides the
[`kde_bandwidth()`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
function for estimating the bandwidth of a kernel density estimate,
[`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)
for constructing the distribution, and
[`gg_density()`](https://pkg.robjhyndman.com/weird/reference/gg_density.md)
for plotting the resulting density. The figure below shows the kernel
density estimate of the `duration` variable obtained using these
functions.

``` r
of <- oldfaithful |>
  filter(duration < 3600, waiting < 7200)
dist_kde(of$duration) |>
  gg_density(show_points = TRUE, jitter = TRUE) +
  labs(x = "Duration (seconds)")
```

![](reference/figures/README-of-density-1.png)

The same functions also work with bivariate data. The figure below shows
the kernel density estimate of the `duration` and `waiting` variables.

``` r
of |>
  select(duration, waiting) |>
  dist_kde() |>
  gg_density(show_points = TRUE, alpha = 0.15) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
```

![](reference/figures/README-of-density2-1.png)

## Statistical tests

Some old methods of anomaly detection used statistical tests. While
these are not recommended, they are still widely used, and are provided
in the package for comparison purposes.

``` r
of |> filter(peirce_anomalies(duration))
#> # A tibble: 2 × 4
#>   time                recorded_duration duration waiting
#>   <dttm>              <chr>                <dbl>   <dbl>
#> 1 2018-04-25 19:08:00 1s                       1    5700
#> 2 2022-12-07 17:19:00 ~4 30s                  30    5220
of |> filter(chauvenet_anomalies(duration))
#> # A tibble: 2 × 4
#>   time                recorded_duration duration waiting
#>   <dttm>              <chr>                <dbl>   <dbl>
#> 1 2018-04-25 19:08:00 1s                       1    5700
#> 2 2022-12-07 17:19:00 ~4 30s                  30    5220
of |> filter(grubbs_anomalies(duration))
#> # A tibble: 1 × 4
#>   time                recorded_duration duration waiting
#>   <dttm>              <chr>                <dbl>   <dbl>
#> 1 2018-04-25 19:08:00 1s                       1    5700
of |> filter(dixon_anomalies(duration))
#> # A tibble: 0 × 4
#> # ℹ 4 variables: time <dttm>, recorded_duration <chr>, duration <dbl>, waiting <dbl>
```

In this example, they only detect the tiny 1-second duration, which is
almost certainly a recording error. An explanation of these tests is
provided in [Chapter 4 of the
book](https://otexts.com/weird/04-tests.html)

## Boxplots

Boxplots are widely used for anomaly detection. Here are three
variations of boxplots applied to the `duration` variable.

``` r
of |>
  ggplot(aes(x = duration)) +
  geom_boxplot() +
  scale_y_discrete() +
  labs(y = "", x = "Duration (seconds)")
```

![](reference/figures/README-of-boxplot-1.png)

``` r
of |> gg_hdrboxplot(duration) +
  labs(x = "Duration (seconds)")
```

![](reference/figures/README-of-boxplot-2.png)

``` r
of |> gg_hdrboxplot(duration, show_points = TRUE) +
  labs(x = "Duration (seconds)")
```

![](reference/figures/README-of-boxplot-3.png)

The latter two plots are highest density region (HDR) boxplots, which
allow the bimodality of the data to be seen. The dark shaded region
contains 50% of the observations, while the lighter shaded region
contains 99% of the observations. The plots use vertical jittering to
reduce overplotting, and highlight potential outliers (those points
lying outside the 99% HDR which have surprisal probability less than
0.0005). An explanation of these plots is provided in [Chapter 5 of the
book](https://otexts.com/weird/05-boxplots.html).

It is also possible to produce bivariate boxplots. Several variations
are provided in the package. Here are two types of bagplot.

``` r
of |>
  gg_bagplot(duration, waiting) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
```

![](reference/figures/README-of-boxplot2-1.png)

``` r
of |>
  gg_bagplot(duration, waiting, scatterplot = TRUE) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
```

![](reference/figures/README-of-boxplot2-2.png)

And here are two types of HDR boxplot

``` r
of |>
  gg_hdrboxplot(duration, waiting) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
```

![](reference/figures/README-of-boxplot3-1.png)

``` r
of |>
  gg_hdrboxplot(duration, waiting, scatterplot = TRUE) +
  labs(x = "Duration (seconds)", y = "Waiting time (seconds)")
```

![](reference/figures/README-of-boxplot3-2.png)

The latter two plots show possible outliers in black (again, defined as
points outside the 99% HDR which have surprisal probability less than
0.0005).

## Scoring functions

Several functions are provided for providing anomaly scores for all
observations.

- The
  [`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
  function uses either a fitted statistical model, or a kernel density
  estimate, to compute density scores.
- The
  [`stray_scores()`](https://pkg.robjhyndman.com/weird/reference/stray_scores.md)
  function uses the stray algorithm to compute anomaly scores.
- The
  [`lof_scores()`](https://pkg.robjhyndman.com/weird/reference/lof_scores.md)
  function uses local outlier factors to compute anomaly scores.
- The
  [`glosh_scores()`](https://pkg.robjhyndman.com/weird/reference/glosh_scores.md)
  function uses the Global-Local Outlier Score from Hierarchies
  algorithm to compute anomaly scores.
- The
  [`lookout_prob()`](https://pkg.robjhyndman.com/weird/reference/lookout_prob.md)
  function uses the lookout algorithm of [Kandanaarachchi & Hyndman
  (2022)](https://robjhyndman.com/publications/lookout/) to compute
  anomaly probabilities.

Here are the top 0.02% most anomalous observations identified by each of
the methods.

``` r
of |>
  mutate(
    surprisal = surprisals(cbind(duration, waiting), probability = FALSE),
    strayscore = stray_scores(cbind(duration, waiting)),
    lofscore = lof_scores(cbind(duration, waiting), k = 150),
    gloshscore = glosh_scores(cbind(duration, waiting)),
    lookout = lookout_prob(cbind(duration, waiting))
  ) |>
  filter(
    surprisal > quantile(surprisal, prob = 0.998) |
      strayscore > quantile(strayscore, prob = 0.998) |
      lofscore > quantile(lofscore, prob = 0.998) |
      gloshscore > quantile(gloshscore, prob = 0.998) |
      lookout < 0.002
  ) |>
  arrange(lookout)
#> # A tibble: 10 × 9
#>    time                recorded_duration   duration waiting surprisal strayscore lofscore gloshscore
#>    <dttm>              <chr>                  <dbl>   <dbl>     <dbl>      <dbl>    <dbl>      <dbl>
#>  1 2018-04-25 19:08:00 1s                         1    5700      17.4     0.150      3.78          1
#>  2 2022-12-03 16:20:00 ~4m                      240    3060      17.4     0.265      2.05          1
#>  3 2022-12-07 17:19:00 ~4 30s                    30    5220      17.1     0.273      1.89          1
#>  4 2020-09-04 01:38:00 >1m 50s                  110    6240      17.1     0.167      1.84          1
#>  5 2023-07-04 12:03:00 ~1 minute 55ish se…       60    4920      17.1     0.122      1.62          1
#>  6 2020-06-01 21:04:00 2 minutes                120    6060      16.8     0.132      2.01          1
#>  7 2023-05-26 00:53:00 4m45s                    285    7140      15.2     0.0761     2.57          1
#>  8 2017-09-22 18:51:00 ~281s                    281    7140      15.0     0.0683     2.57          1
#>  9 2023-08-09 20:52:00 4m39s                    279    7140      14.9     0.0651     2.57          1
#> 10 2018-09-22 16:37:00 ~4m13s                   253    7140      14.7     0.0194     2.57          1
#> # ℹ 1 more variable: lookout <dbl>
```

The
[`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
function can also compute the probability of obtaining surprisal values
at least as extreme as those observed. In fact, this is the default
behaviour, obtained when `probability = TRUE`.

``` r
of |>
  mutate(
    surprisal = surprisals(cbind(duration, waiting), probability = FALSE),
    prob = surprisals(cbind(duration, waiting))
  ) |>
  arrange(prob)
#> # A tibble: 2,097 × 6
#>    time                recorded_duration       duration waiting surprisal     prob
#>    <dttm>              <chr>                      <dbl>   <dbl>     <dbl>    <dbl>
#>  1 2018-04-25 19:08:00 1s                             1    5700      17.4 0.000477
#>  2 2022-12-03 16:20:00 ~4m                          240    3060      17.4 0.000954
#>  3 2023-07-04 12:03:00 ~1 minute 55ish seconds       60    4920      17.1 0.00143 
#>  4 2022-12-07 17:19:00 ~4 30s                        30    5220      17.1 0.00191 
#>  5 2020-09-04 01:38:00 >1m 50s                      110    6240      17.1 0.00238 
#>  6 2020-06-01 21:04:00 2 minutes                    120    6060      16.8 0.00286 
#>  7 2021-01-22 18:35:00 2m50s                        170    3600      16.7 0.00334 
#>  8 2022-11-29 14:51:00 ~3m                          180    3840      16.4 0.00381 
#>  9 2020-08-31 09:56:00 ~2m50s                       170    3840      16.4 0.00429 
#> 10 2020-07-23 23:17:00 3m06s                        186    4320      16.3 0.00477 
#> # ℹ 2,087 more rows
```

## Robust multivariate scaling

Some anomaly detection methods require the data to be scaled first, so
all observations are on the same scale. However, many scaling methods
are not robust to anomalies. The
[`mvscale()`](https://sevvandi.github.io/lookout/reference/mvscale.html)
function provides a multivariate robust scaling method, that optionally
takes account of the relationships betwen variables, and uses robust
estimates of center, scale and covariance by default. The centers are
removed using medians, the scale function is the IQR, and the covariance
matrix is estimated using a robust OGK estimate. The data are scaled
using the Cholesky decomposition of the inverse covariance. Then the
scaled data are returned. The scaled variables are rotated to be
orthogonal, so are renamed as `z1`, `z2`, etc. Non-rotated scaling is
possible by setting `cov = NULL`.

``` r
mvscale(of)
#> Warning in mvscale(of): Ignoring non-numeric columns: time, recorded_duration
#> # A tibble: 2,097 × 4
#>    time                recorded_duration      z1     z2
#>    <dttm>              <chr>               <dbl>  <dbl>
#>  1 2017-01-14 00:06:00 3m 16s            -2.20    0.332
#>  2 2017-01-26 14:27:00 ~4m               -0.0431  0.111
#>  3 2017-01-27 23:57:00 2m 1s             -4.27   -3.43 
#>  4 2017-01-30 15:09:00 ~4m                0.345  -0.886
#>  5 2017-01-31 13:27:00 ~3.5m             -1.28   -0.332
#>  6 2017-01-31 15:00:00 ~4m                0       0    
#>  7 2017-02-03 23:13:00 3m 25s            -1.22   -1.11 
#>  8 2017-02-04 22:14:00 3m 34s            -0.966  -0.665
#>  9 2017-02-05 17:19:00 4m 0s             -0.215   0.554
#> 10 2017-02-05 19:00:00 4m 2s             -0.121   0.554
#> # ℹ 2,087 more rows
mvscale(of, cov = NULL)
#> Warning in mvscale(of, cov = NULL): Ignoring non-numeric columns: time, recorded_duration
#> # A tibble: 2,097 × 4
#>    time                recorded_duration duration waiting
#>    <dttm>              <chr>                <dbl>   <dbl>
#>  1 2017-01-14 00:06:00 3m 16s             -1.98     0.338
#>  2 2017-01-26 14:27:00 ~4m                 0        0.113
#>  3 2017-01-27 23:57:00 2m 1s              -5.37    -3.50 
#>  4 2017-01-30 15:09:00 ~4m                 0       -0.902
#>  5 2017-01-31 13:27:00 ~3.5m              -1.35    -0.338
#>  6 2017-01-31 15:00:00 ~4m                 0        0    
#>  7 2017-02-03 23:13:00 3m 25s             -1.58    -1.13 
#>  8 2017-02-04 22:14:00 3m 34s             -1.17    -0.676
#>  9 2017-02-05 17:19:00 4m 0s               0        0.564
#> 10 2017-02-05 19:00:00 4m 2s               0.0902   0.564
#> # ℹ 2,087 more rows
```
