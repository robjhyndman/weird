# Surprisals and surprisal probabilities

A surprisal is given by \\s = -\log f(y)\\ where \\f\\ is the density or
probability mass function of the estimated or assumed distribution, and
\\y\\ is an observation. This is returned by `surprisals()`. A surprisal
probability is the probability of a surprisal at least as extreme as
\\s\\. This is returned by `surprisals_prob()`

The surprisal probabilities may be computed in three different ways.

1.  Given the same distribution that was used to compute the surprisal
    values (when `approximation = "none"`). Under this option, surprisal
    probabilities are equal to 1 minus the coverage probability of the
    largest HDR that contains each value. Surprisal probabilities
    smaller than 1e-6 are returned as 1e-6.

2.  Using a Generalized Pareto Distribution fitted to the most extreme
    surprisal values (those with probability less than
    `threshold_probability`). This option is used when
    `approximation = "gdp"`. For surprisal probabilities greater than
    `threshold_probability`, the value of `threshold_probability` is
    returned. Under this option, the distribution is used for computing
    the surprisal values but not for determining their probabilities.
    Due to extreme value theory, the resulting probabilities should be
    relatively insensitive to the distribution used in computing the
    surprisal values.

3.  Using ranks (`approximation = "rank"`), the approximate probability
    is the proportion of observations with greater surprisal values.
    This is also insensitive to the distribution used in computing the
    surprisal values.

## Usage

``` r
surprisals(object, ...)

surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
  threshold_probability = 0.1,
  ...
)
```

## Arguments

- object:

  A model or numerical data set

- ...:

  Other arguments are passed to the appropriate method.

- approximation:

  Character string specifying the approximation to use in computing the
  surprisal probabilities. Ignored if `probability = FALSE`.
  `approximation = "none"` specifies that no approximation is to be
  used; `approximation = "gpd"` (default) specifies that the Generalized
  Pareto distribution should be used; while `approximation = "rank"`
  specifies that the probabilities should be estimated based on the rank
  of the surprisal values.

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"` and `probability = TRUE`.

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## References

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Chapter
6, <https://OTexts.com/weird/>.

## Author

Rob J Hyndman

## Examples

``` r
# surprisals computed from bivariate data set
oldfaithful |>
  mutate(
    loo_fscores = surprisals_prob(cbind(duration, waiting), loo = TRUE)
  )
#> # A tibble: 2,097 × 5
#>    time                recorded_duration duration waiting loo_fscores
#>    <dttm>              <chr>                <dbl>   <dbl>       <dbl>
#>  1 2017-01-14 00:06:00 3m 16s                 196    5940      0.0735
#>  2 2017-01-26 14:27:00 ~4m                    240    5820      0.1   
#>  3 2017-01-27 23:57:00 2m 1s                  121    3900      0.1   
#>  4 2017-01-30 15:09:00 ~4m                    240    5280      0.1   
#>  5 2017-01-31 13:27:00 ~3.5m                  210    5580      0.1   
#>  6 2017-01-31 15:00:00 ~4m                    240    5760      0.1   
#>  7 2017-02-03 23:13:00 3m 25s                 205    5160      0.1   
#>  8 2017-02-04 22:14:00 3m 34s                 214    5400      0.1   
#>  9 2017-02-05 17:19:00 4m 0s                  240    6060      0.1   
#> 10 2017-02-05 19:00:00 4m 2s                  242    6060      0.1   
#> # ℹ 2,087 more rows
```
