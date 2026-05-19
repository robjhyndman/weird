# weird (development version)

* Added `fetch_air_quality()` to download Beijing air quality data from 12 monitoring stations (2013-2017).
* Added `gun_deaths` dataset containing firearm homicide rates and gun ownership by country for 2017.
* `wine_reviews` columns have been reordered.
* `lof_scores()` now returns 1 (instead of 0) when there are too many duplicates.
* `mvscale()` now better handles missing and infinite values.

# weird 2.1.0

* Added `surprisals()` and `surprisals_prob()` for `glm` objects
* Faster calculation of `surprisals_prob()` for `lm` and `gam` objects
* More accurate rank-based surprisal calculation when ties and missings present
* More accurate HDR calculations
* Added covariance calculation for multivariate `dist_kde` objects
* Added `distributional::hdr()` and `distributional::parameters()` methods for `dist_kde` objects
* Faster calculation for `kde_bandwidth()` with `method = "lookout"`, now using [mlpack](https://www.mlpack.org/) for merge distances
* `mvscale()` re-added to [weird](https://pkg.robjhyndman.com/weird/)
* Dropped [lookout](https://sevvandi.github.io/lookout/) package dependency
* Added lots of unit tests
* Better docs
* Bug fixes

# weird 2.0.0

* Added `fr_mortality` data set
* Updated `oldfaithful` and `cricket_batting` data sets
* Refactored package to use [distributional](https://pkg.robjhyndman.com/distributional/) objects
* Added `dist_kde()` and `dist_density()`
* Removed `as_kde()` and `autoplot.kde()`
* Rewrote `kde_bandwidth()` to handle more methods
* Added `gg_density()` and `gg_density_layer()`
* Replaced `density_scores()` with `surprisals()` and added `surprisals_prob()`
* Updated `gg_bagplot()` and `gg_hdrboxplot()` to use `show_points` argument
* Removed `lookout()`
* Added `hampel_anomalies()`
* `mvscale()` moved to [lookout](https://sevvandi.github.io/lookout/) package
* Dropped `interpolation` dependency
* No longer exporting `weird_packages()` and `weird_conflicts()`

# weird 1.0.2

* Removed `wine_reviews` dataset and created `fetch_wine_reviews()` function.
* Bug fixes.

# weird 1.0.0

* Initial CRAN submission.
