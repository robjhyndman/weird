# Wine prices and points

A data set containing data on wines from 44 countries, taken from *Wine
Enthusiast Magazine* during the week of 15 June 2017. The data are
downloaded and returned.

## Usage

``` r
fetch_wine_reviews()
```

## Format

A data frame with 110,203 rows and 8 columns:

- country:

  Country of origin

- state:

  State or province of origin

- region:

  Region of origin

- winery:

  Name of vineyard that made the wine

- variety:

  Variety of grape

- points:

  Points allocated by WineEnthusiast reviewer on a scale of 0-100

- price:

  Price of a bottle of wine in \$US

- year:

  Year of wine extracted from `title`

## Source

<https://kaggle.com>

## Value

Data frame

## Examples

``` r
if (FALSE) { # \dontrun{
wine_reviews <- fetch_wine_reviews()
wine_reviews |>
  ggplot(aes(x = points, y = price)) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.1) +
  scale_y_log10()
} # }
```
