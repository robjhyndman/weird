# Changelog

## weird (development version)

- Added surprisals() and surprisals_prob() for glm objects
- Faster calculation of surprisals_prob() for lm and gam objects
- Faster calculation for kde_bandwidth(method = “lookout”)
- Added covariance calculation for multivariate dist_kde objects
- More accurate HDR calculations
- More accurate rank-based surprisal calculation when ties and missings
  present
- Added lots of unit tests
- Better docs
- Bug fixes

## weird 2.0.0

CRAN release: 2026-01-27

- Added fr_mortality data set
- Updated oldfaithful and cricket_batting data sets
- Refactored package to use distributional objects
- Added dist_kde() and dist_density()
- Removed as_kde() and autoplot.kde()
- Rewrote kde_bandwidth() to handle more methods
- Added gg_density() and gg_density_layer()
- Replaced density_scores() with surprisals() and added
  surprisals_prob()
- Updated gg_bagplot() and gg_hdrboxplot() to use show_points argument
- Removed lookout()
- Added hampel_anomalies()
- mvscale() moved to lookout package
- Dropped interpolation dependency
- No longer exporting weird_packges() and weird_conflicts()

## weird 1.0.2

CRAN release: 2024-01-24

- Removed wine_reviews dataset and created
  [`fetch_wine_reviews()`](https://pkg.robjhyndman.com/weird/reference/fetch_wine_reviews.md)
  function.
- Bug fixes.

## weird 1.0.0

CRAN release: 2024-01-12

- Initial CRAN submission.
