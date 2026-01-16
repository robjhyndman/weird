# Changelog

## weird (development version)

- Added fr_mortality data set
- Updated oldfaithful and cricket_batting data sets
- Refactored package to use distributional objects
- Removed as_kde() and autoplot.kde()
- Added gg_density() and gg_density_layer()
- Added surprisals() and surprisal_prob()
- Renamed lookout() to lookout_prob()
- Rewrote kde_bandwidth() to handle more methods
- Added hampel_anomalies()
- mvscale() moved to lookout package
- Dropped interpolation dependency

## weird 1.0.2

CRAN release: 2024-01-24

- Removed wine_reviews dataset and created
  [`fetch_wine_reviews()`](https://pkg.robjhyndman.com/weird/reference/fetch_wine_reviews.md)
  function.
- Bug fixes.

## weird 1.0.0

CRAN release: 2024-01-12

- Initial CRAN submission.
