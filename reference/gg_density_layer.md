# Add ggplot layer of densities from distributional objects in 1 dimension

Add ggplot layer of densities from distributional objects in 1 dimension

## Usage

``` r
gg_density_layer(object, scale = 1, ngrid = 501, ...)
```

## Arguments

- object:

  distribution object from the distributional package or
  [`dist_kde`](https://pkg.robjhyndman.com/weird-package/reference/dist_kde.md)()

- scale:

  Scaling factor for the density function.

- ngrid:

  Number of grid points to use for the density function.

- ...:

  Additional arguments are passed to
  [`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Value

A ggplot layer

## Details

This function adds a ggplot layer of a density from a distributional
object. For univariate densities, it adds a line plot of the density
function. For bivariate densities, it adds a contour plot of the density
function.

## Author

Rob J Hyndman

## Examples

``` r
dist_mixture(
  dist_normal(-2, 1),
  dist_normal(2, 1),
  weights = c(1 / 3, 2 / 3)
) |>
  gg_density() +
  gg_density_layer(dist_normal(-2, 1), linetype = "dashed", scale = 1 / 3) +
  gg_density_layer(dist_normal(2, 1), linetype = "dashed", scale = 2 / 3)
```
