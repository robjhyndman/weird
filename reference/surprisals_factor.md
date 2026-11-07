# Surprisals and surprisal probabilities of categorical data

For a categorical variable taking one of the categories
\\c_1,\dots,c_K\\, the surprisal of an observation \\y\\ is \\s = -\log
f(y)\\, where \\f(c_k)\\ is the probability of category \\c_k\\. The
surprisal probability is the total probability of all categories that
are no more probable than \\y\\: \$\$p = \sum\_{k=1}^K f(c_k)
1\big(f(c_k) \le f(y)\big).\$\$ Tied categories (with the same
probability) have the same surprisal probability, which includes the
probabilities of all of them.

## Usage

``` r
# S3 method for class 'factor'
surprisals(object, prob = NULL, prior = 0, loo = FALSE, ...)

# S3 method for class 'factor'
surprisals_prob(
  object,
  approximation = c("none", "empirical", "rank"),
  threshold_probability = 0.1,
  prob = NULL,
  prior = 0,
  loo = FALSE,
  ...
)
```

## Arguments

- object:

  A factor.

- prob:

  A numeric vector of probabilities for the levels of `object`, either
  named by level or in the order of `levels(object)`. If `NULL` (the
  default), the probabilities are estimated from `object`.

- prior:

  A non-negative constant \\a\\ added to the count of each level when
  estimating the probabilities. Only used when `prob` is `NULL`.

- loo:

  Should leave-one-out surprisals be computed? Only used when `prob` is
  `NULL`.

- ...:

  Other arguments are ignored.

- approximation:

  Character string specifying the method to use in computing the
  surprisal probabilities. With `"none"` (the default), the exact
  probabilities are computed from the category probabilities. With
  `"empirical"` (or `"rank"`), the surprisal probability of each
  observation is the proportion of observations with greater or equal
  surprisals.

- threshold_probability:

  Not used for categorical data.

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

By default, the probabilities are estimated from the data as
\\\hat{f}(c_k) = (n_k + a)/(n + Ka)\\, where \\n_k\\ is the number of
observations in category \\c_k\\, \\n\\ is the number of non-missing
observations, \\K\\ is the number of levels of `object` (including
levels with no observations), and \\a\\ is `prior`. This is the
posterior mean of \\f(c_k)\\ under a symmetric Dirichlet prior with
parameter \\a\\. With `loo = TRUE`, each observation is omitted when
estimating the probabilities used for its own surprisal and surprisal
probability, so that \\n_k\\ and \\n\\ are each reduced by one for an
observation in category \\c_k\\.

To compute surprisals of several categorical variables jointly, combine
them into a single factor with
[`interaction()`](https://rdrr.io/r/base/interaction.html).

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter
7, <https://OTexts.com/weird/density.html>.

Hyndman, R J & Frazier, D T (2026) "Anomaly detection using surprisals",
<https://robjhyndman.com/publications/surprisals.html>.

## Author

Rob J Hyndman

## Examples

``` r
# Road types of Australian road deaths since 2014
road_deaths |>
  filter(!is.na(road_type)) |>
  mutate(
    s = surprisals(road_type),
    p = surprisals_prob(road_type)
  ) |>
  count(road_type, s, p) |>
  arrange(p)
#> # A tibble: 8 × 4
#>   road_type                     s        p     n
#>   <fct>                     <dbl>    <dbl> <int>
#> 1 Busway                     8.74 0.000160     2
#> 2 Pedestrian Thoroughfare    7.64 0.000638     6
#> 3 Access road                4.88 0.00822     95
#> 4 Collector Road             2.56 0.0854     968
#> 5 Sub-arterial Road          1.80 0.251     2077
#> 6 Local Road                 1.54 0.465     2677
#> 7 Arterial Road              1.45 0.698     2929
#> 8 National or State Highway  1.20 1         3782
# Joint surprisal probabilities of road user and sex
road_deaths |>
  filter(!is.na(road_user), !is.na(gender)) |>
  mutate(p = surprisals_prob(interaction(road_user, gender))) |>
  count(road_user, gender, p) |>
  arrange(p)
#> # A tibble: 12 × 4
#>    road_user            gender       p     n
#>    <fct>                <fct>    <dbl> <int>
#>  1 Motorcycle passenger Male   0.00298   173
#>  2 Pedal cyclist        Female 0.00666   213
#>  3 Motorcycle passenger Female 0.0104    218
#>  4 Motorcycle rider     Female 0.0153    286
#>  5 Pedal cyclist        Male   0.0392   1382
#>  6 Pedestrian           Female 0.0891   2893
#>  7 Pedestrian           Male   0.194    6064
#>  8 Passenger            Female 0.300    6194
#>  9 Driver               Female 0.412    6464
#> 10 Passenger            Male   0.531    6906
#> 11 Motorcycle rider     Male   0.659    7428
#> 12 Driver               Male   1       19761
```
