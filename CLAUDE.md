# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

`weird` is an R package providing anomaly detection functions and
datasets to accompany the textbook *“That’s Weird: Anomaly Detection
Using R”* by Rob J Hyndman. It wraps and extends several statistical and
ML methods with a tidy, ggplot2-compatible API.

## Common Commands

``` r

# Load package for interactive development
devtools::load_all()

# Generate NAMESPACE and man/*.Rd from Roxygen2 comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
devtools::test(filter = "surprisals_data")

# Full R CMD CHECK (used in CI)
devtools::check()
```

Via Makefile (shortcuts for the above):

``` bash
make docs     # devtools::document()
make test     # devtools::test()
make check    # devtools::check()
make install  # devtools::install()
make pkgdown  # pkgdown::build_site()
```

README is generated from `README.qmd` (Quarto), not edited directly.

## Architecture

### Core concept: surprisals

The central abstraction is the *surprisal* — the negative log-density
−log f(y) of an observation under a fitted model or estimated density.
High surprisal = anomalous.
[`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
and
[`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)
are S3 generics with methods for: - Raw data: `.numeric`, `.matrix`,
`.data.frame` (fits a KDE internally) - Fitted models: `.lm`, `.glm`,
`.gam`

### Distributional integration

Kernel density estimates are wrapped as `distributional` objects via
[`dist_kde()`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)
and
[`dist_density()`](https://pkg.robjhyndman.com/weird/reference/dist_density.md).
This gives them a standard interface
([`density()`](https://rdrr.io/r/stats/density.html), `cdf()`,
[`quantile()`](https://rdrr.io/r/stats/quantile.html),
[`mean()`](https://rdrr.io/r/base/mean.html), etc.) and lets them
compose with the rest of the distributional ecosystem.

### Visualization layer

All plot functions return `ggplot2` objects and follow a `gg_*` naming
convention: -
[`gg_density()`](https://pkg.robjhyndman.com/weird/reference/gg_density.md)
— KDE density plots (1D or 2D) -
[`gg_density_layer()`](https://pkg.robjhyndman.com/weird/reference/gg_density_layer.md)
— add density contours as a ggplot2 layer -
[`gg_hdrboxplot()`](https://pkg.robjhyndman.com/weird/reference/gg_hdrboxplot.md)
— Highest Density Region boxplot -
[`gg_bagplot()`](https://pkg.robjhyndman.com/weird/reference/bagplot.md)
— bivariate bagplot

### File organisation

| File(s) | Responsibility |
|----|----|
| `surprisals.R`, `surprisals_data.R`, `surprisals_models.R` | Generic + S3 methods for surprisal computation |
| `dist_kde.R`, `dist_density.R` | `distributional`-compatible KDE objects |
| `kde_bandwidth.R` | Bandwidth selection (robust, normal, plugin, lookout) |
| `hdr.R` | Highest Density Region calculation and HDR boxplot |
| `gg_density.R`, `gg_density_layer.R` | KDE visualisation |
| `bagplot.R` | Bivariate bagplot (`gg_bagplot`) |
| `stray.R`, `lof_scores.R` | Modern scoring methods (STray, LOF, GLOSH) |
| `grubbs.R`, `hampel.R`, `peirce.R`, `dixon.R` | Classical/legacy outlier tests |
| `mvscale.R` | Robust multivariate scaling (OGK + Cholesky) |
| `attach.R`, `conflicts.R`, `zzz.R` | Package startup — auto-attaches dplyr, ggplot2, distributional |

### Documentation

All user-facing functions use Roxygen2 with markdown
(`Roxygen: list(markdown = TRUE)`). After editing function signatures or
`@export` tags, always run `devtools::document()` to regenerate
`NAMESPACE` and `man/`. Never hand-edit those files.

### Testing

Tests live in `tests/testthat/` and use testthat edition 3. Test file
names mirror source files (e.g. `test_surprisals_data.R` tests
`surprisals_data.R`). Run a focused subset with the `filter` argument
matching the filename suffix.
