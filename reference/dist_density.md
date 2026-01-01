# Create distributional object based on a specified density

Creates a distributional object using a density specified as pair of
vectors giving (x, f(x)). The density is assumed to be piecewise linear
between the points provided, and 0 outside the range of x.

## Usage

``` r
dist_density(x, density)
```

## Arguments

- x:

  Numerical vector of ordinates, or a list of such vectors.

- density:

  Numerical vector of density values, or a list of such vectors.

## Examples

``` r
dist_density(seq(-4, 4, by = 0.01), dnorm(seq(-4, 4, by = 0.01)))
#> <distribution[1]>
#> [1] density[801]
```
