# Biplot of a two-dimensional projection

Draw a two-dimensional projection of the scores with the original
variable axes overlaid as arrows (loadings), as in a biplot. Pass
`object` (the output of
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) or an rrcov
`Pca*` function) to take the scores and loadings from it; otherwise
supply `scores` and `loadings` directly. The arrows are stretched by a
common factor so that the longest arrow just reaches the edge of the
point cloud, and only loadings longer than `label_threshold` are
labelled.

## Usage

``` r
biplot_projection(
  object = NULL,
  scores = NULL,
  loadings = NULL,
  label_threshold = 0
)
```

## Arguments

- object:

  Optionally, the output of
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) or an rrcov
  `Pca*` function. If supplied, the scores and loadings are extracted
  from it and the `scores` and `loadings` arguments are ignored.

- scores:

  A matrix or data frame of scores, with the first two columns used as
  the horizontal and vertical coordinates. Ignored if `object` is
  supplied.

- loadings:

  A matrix or data frame of loadings, with row names giving the variable
  names and the first two columns used as the arrow directions. Ignored
  if `object` is supplied.

- label_threshold:

  Only loadings whose squared length exceeds this threshold are
  labelled. The default of `0` labels every loading.

## Value

A `ggplot` object.

## Author

Rob J Hyndman

## Examples

``` r
pca <- prcomp(oldfaithful[, c("duration", "waiting")], scale = TRUE)
biplot_projection(pca)
```
