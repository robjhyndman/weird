# Convert Gaussian mixture model to a distributional object

Convert Gaussian mixture model to a distributional object

## Usage

``` r
dist_mclust(object)
```

## Arguments

- object:

  Object of class `Mclust`, output from the
  \[[mclust::Mclust](https://mclust-org.github.io/mclust/reference/Mclust.html)\]
  function

## Value

An object of class `distributional`

## Examples

``` r
if (FALSE) { # \dontrun{
library(mclust)
# Create a bivariate Gaussian mixture model for the Old Faithful data
gmm <- oldfaithful |>
  select(duration, waiting) |>
  Mclust() |>
  gmm_to_dist()
gg_density(gmm) +
  geom_point(data = oldfaithful, aes(x = duration, y = waiting), alpha = 0.1) +
  labs(x = "Duration", y = "Waiting time")
} # }
```
