# Augment data with results from a robust principal component analysis

Augment the data with information from an rrcov `Pca*` object (such as
the output of
[`rrcov::PcaHubert()`](https://rdrr.io/pkg/rrcov/man/PcaHubert.html) or
[`rrcov::PcaClassic()`](https://rdrr.io/pkg/rrcov/man/PcaClassic.html)).
The returned tibble contains the principal component scores
(`.fittedPC1`, `.fittedPC2`, ...), the score distance (`.sd`) and the
orthogonal distance (`.od`) of each observation. The score distance
measures how far an observation lies from the centre *within* the
projection subspace, while the orthogonal distance measures how far it
lies *from* the subspace. If `data` is supplied, its columns are
returned alongside these results.

## Usage

``` r
# S3 method for class 'Pca'
augment(x, data = NULL, ...)
```

## Arguments

- x:

  An rrcov `Pca*` object.

- data:

  The original data matrix or data frame used to compute the PCA. If
  supplied, its columns are bound to the left of the returned tibble.

- ...:

  Unused.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per observation.

## Author

Rob J Hyndman

## Examples

``` r
Y <- oldfaithful[, c("duration", "waiting")]
pca <- rrcov::PcaHubert(as.matrix(Y), k = 1)
broom::augment(pca, data = Y)
#> # A tibble: 2,097 × 5
#>    duration waiting .fittedPC1    .sd   .od
#>       <dbl>   <dbl>      <dbl>  <dbl> <dbl>
#>  1      196    5940      -98.6 0.224  49.9 
#>  2      240    5820       20.9 0.0474  4.42
#>  3      121    3900     1942.  4.41   99.0 
#>  4      240    5280      561.  1.27    2.45
#>  5      210    5580      261.  0.593  31.4 
#>  6      240    5760       80.9 0.183   3.65
#>  7      205    5160      681.  1.55   31.0 
#>  8      214    5400      441.  1.00   25.1 
#>  9      240    6060     -219.  0.497   7.47
#> 10      242    6060     -219.  0.497   5.47
#> # ℹ 2,087 more rows
```
