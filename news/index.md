# Changelog

## weird (development version)

## weird 2.1.0

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
  and
  [`gg_density_layer()`](https://pkg.robjhyndman.com/weird/reference/gg_density_layer.md)
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
