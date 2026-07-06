# Changelog

## weird 3.0.0

- Moved the Old Faithful Geyser examples from the README into a new
  vignette,
  [`vignette("old-faithful")`](https://pkg.robjhyndman.com/weird/articles/old-faithful.md).
- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  method added for rrcov `Pca*` objects, returning the principal
  component scores, score distances and orthogonal distances, optionally
  alongside the original data.
- [`dist_mclust()`](https://pkg.robjhyndman.com/weird/reference/dist_mclust.md)
  added to convert a Gaussian mixture model (`Mclust` object) to a
  distributional object.
- [`density_df()`](https://pkg.robjhyndman.com/weird/reference/density_df.md)
  added, converting a distributional object to a data frame containing
  density values over a grid, suitable for plotting.
- [`biplot_projection()`](https://pkg.robjhyndman.com/weird/reference/biplot_projection.md)
  added, drawing a two-dimensional projection of scores with the
  original variable axes overlaid as arrows.
- [`outlier_map()`](https://pkg.robjhyndman.com/weird/reference/outlier_map.md)
  added, drawing the score distance against the orthogonal distance for
  a `prcomp` or rrcov `Pca*` object, with optional cutoff thresholds
  classifying observations as regular, good leverage points, orthogonal
  outliers or bad leverage points.
- Added
  [`fetch_air_quality()`](https://pkg.robjhyndman.com/weird/reference/fetch_air_quality.md)
  to download Beijing air quality data from 12 monitoring stations
  (2013-2017).
- Added `gun_deaths` dataset containing firearm homicide rates and gun
  ownership by country for 2017.
- `wine_reviews` columns have been reordered.
- `gg_density_layer()` removed,
- [`lof_scores()`](https://pkg.robjhyndman.com/weird/reference/lof_scores.md)
  now returns 1 (instead of 0) when there are too many duplicates.
- [`mvscale()`](https://pkg.robjhyndman.com/weird/reference/mvscale.md)
  now better handles missing and infinite values, and returns centering
  and scaling terms as attributes.
- [`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)
  is now robust to missing values.
- Removed several dependencies
- Bug fixes and documentation improvements

## weird 2.1.0

CRAN release: 2026-05-05

- Added
  [`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
  and
  [`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
  for `glm` objects
- Faster calculation of
  [`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
  for `lm` and `gam` objects
- More accurate rank-based surprisal calculation when ties and missings
  present
- More accurate HDR calculations
- Added covariance calculation for multivariate `dist_kde` objects
- Added
  [`distributional::hdr()`](https://pkg.mitchelloharawild.com/distributional/reference/hdr.html)
  and
  [`distributional::parameters()`](https://pkg.mitchelloharawild.com/distributional/reference/parameters.html)
  methods for `dist_kde` objects
- Faster calculation for
  [`kde_bandwidth()`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
  with `method = "lookout"`, now using [mlpack](https://www.mlpack.org/)
  for merge distances
- [`mvscale()`](https://pkg.robjhyndman.com/weird/reference/mvscale.md)
  re-added to [weird](https://pkg.robjhyndman.com/weird/)
- Dropped [lookout](https://sevvandi.github.io/lookout/) package
  dependency
- Added lots of unit tests
- Better docs
- Bug fixes

## weird 2.0.0

CRAN release: 2026-01-27

- Added `fr_mortality` data set
- Updated `oldfaithful` and `cricket_batting` data sets
- Refactored package to use
  [distributional](https://pkg.robjhyndman.com/distributional/) objects
- Added
  [`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)
  and
  [`dist_density()`](https://pkg.robjhyndman.com/weird/reference/dist_density.md)
- Removed `as_kde()` and `autoplot.kde()`
- Rewrote
  [`kde_bandwidth()`](https://pkg.robjhyndman.com/weird/reference/kde_bandwidth.md)
  to handle more methods
- Added
  [`gg_density()`](https://pkg.robjhyndman.com/weird/reference/gg_density.md)
  and `gg_density_layer()`
- Replaced `density_scores()` with
  [`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
  and added
  [`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
- Updated
  [`gg_bagplot()`](https://pkg.robjhyndman.com/weird/reference/bagplot.md)
  and
  [`gg_hdrboxplot()`](https://pkg.robjhyndman.com/weird/reference/gg_hdrboxplot.md)
  to use `show_points` argument
- Removed `lookout()`
- Added
  [`hampel_anomalies()`](https://pkg.robjhyndman.com/weird/reference/hampel_anomalies.md)
- [`mvscale()`](https://pkg.robjhyndman.com/weird/reference/mvscale.md)
  moved to [lookout](https://sevvandi.github.io/lookout/) package
- Dropped `interpolation` dependency
- No longer exporting `weird_packages()` and `weird_conflicts()`

## weird 1.0.2

CRAN release: 2024-01-24

- Removed `wine_reviews` dataset and created
  [`fetch_wine_reviews()`](https://pkg.robjhyndman.com/weird/reference/fetch_wine_reviews.md)
  function.
- Bug fixes.

## weird 1.0.0

CRAN release: 2024-01-12

- Initial CRAN submission.
