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

[`library(weird)`](https://pkg.robjhyndman.com/weird/) will also load
the following packages:

- [dplyr](https://dplyr.tidyverse.org), for data manipulation.
- [ggplot2](https://ggplot2.tidyverse.org), for data visualisation.
- [distributional](https://cran.r-project.org/package=distributional),
  for handling probability distributions.

When you load the weird package, you get a condensed summary of
conflicts with other packages you have previously loaded:

``` r

library(weird)
#> ── Attaching core weird packages ──────────────────────────────── weird 2.1.0.9000 ──
#> ✔ distributional 0.8.1     ✔ ggplot2        4.0.3
#> ✔ dplyr          1.2.1
#> ── Conflicts ───────────────────────────────────────────────────── weird_conflicts ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```
