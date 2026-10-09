# Fashion-MNIST sneakers

All 1000 images of sneakers from the Fashion-MNIST test set, together
with 10 images from other classes of varying similarity to sneakers:
four ankle boots, three sandals, two bags and one pair of trousers. Each
image is 28 x 28 pixels.

## Usage

``` r
fashion
```

## Format

A data frame with 1010 rows and 4 columns:

- id:

  Index of the image in the Fashion-MNIST test set

- label:

  Class of the image (a factor with the 10 Fashion-MNIST classes)

- planted:

  `TRUE` for the 10 images that are not sneakers

- pixels:

  A 1010 x 784 integer matrix of pixel intensities, from 0 (white) to
  255 (black), with one row per image. Each row contains the 28 x 28
  pixels of an image, stored row by row, so
  `matrix(pixels[i, ], nrow = 28, byrow = TRUE)` gives image `i`.

## Source

Xiao, H., Rasul, K., & Vollgraf, R. (2017). Fashion-MNIST: a novel image
dataset for benchmarking machine learning algorithms.
<https://github.com/zalandoresearch/fashion-mnist> (MIT licence)

## Value

Data frame

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter
13, <https://OTexts.com/weird/>.

## Examples

``` r
fashion |>
  count(label)
#> # A tibble: 5 × 2
#>   label          n
#>   <fct>      <int>
#> 1 Trousers       1
#> 2 Sandal         3
#> 3 Sneaker     1000
#> 4 Bag            2
#> 5 Ankle boot     4
```
