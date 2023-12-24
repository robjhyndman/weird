
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weird <img src="man/figures/weird-hex.png" align="right" width = 150 />

<!-- badges: start -->

[![R build
status](https://github.com/robjhyndman/weird-package/workflows/R-CMD-check/badge.svg)](https://github.com/robjhyndman/weird-package/actions)
<!-- badges: end -->

## Overview

The weird package contains functions and data used in the book [*That’s
Weird: Anomaly Detection Using R*](https://OTexts.com/weird/) by Rob J
Hyndman. It also loads several packages needed to do the analysis
described in the book. These packages work with the
[tidyverse](https://www.tidyverse.org/) set of packages, sharing common
data representations and API design.

## Installation

You can install the development version of weird from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robjhyndman/weird-package")
```

## Usage

`library(weird)` will load the following packages:

- [dplyr](https://dplyr.tidyverse.org), for data manipulation.
- [ggplot2](https://ggplot2.tidyverse.org), for data visualisation.
- [ks](https://cran.r-project.org/package=ks), for fitting models and
  producing forecasts.

You also get a condensed summary of conflicts with other packages you
have loaded:

``` r
library(weird)
#> ── Attaching packages ────────────────────────────────────── weird 0.0.0.9000 ──
#> ✔ dplyr   1.1.4      ✔ ks      1.14.1
#> ✔ ggplot2 3.4.4
#> ── Conflicts ──────────────────────────────────────────────── weird_conflicts ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

## Example

``` r
# TBC
```
