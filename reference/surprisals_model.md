# Surprisals and surprisal probabilities computed from a model

A surprisal is given by \\s = -\log f(y)\\ where \\f\\ is the density or
probability mass function of the estimated or assumed distribution, and
\\y\\ is an observation. This is returned by
[`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md).
A surprisal probability is the probability of a surprisal at least as
extreme as \\s\\. This is returned by
[`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)

## Usage

``` r
# S3 method for class 'lm'
surprisals(object, loo = FALSE, ...)

# S3 method for class 'lm'
surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
  threshold_probability = 0.1,
  loo = FALSE,
  ...
)

# S3 method for class 'gam'
surprisals(object, ...)

# S3 method for class 'gam'
surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
  threshold_probability = 0.1,
  ...
)
```

## Arguments

- object:

  A model object such as returned by
  [`lm`](https://rdrr.io/r/stats/lm.html),
  [`glm`](https://rdrr.io/r/stats/glm.html), or
  [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html). This includes a
  specified conditional probability distribution which is used to
  compute surprisal values.

- loo:

  Should leave-one-out surprisals be computed? For computational
  reasons, this is only available for `lm` objects.

- ...:

  Other arguments are ignored.

- approximation:

  Character string specifying the method to use in computing the
  surprisal probabilities. See Details below.

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"`.

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

The surprisal probabilities may be computed in three different ways.

1.  Given the same distribution that was used to compute the surprisal
    values (when `approximation = "none"`). Under this option, surprisal
    probabilities are equal to 1 minus the coverage probability of the
    largest HDR that contains each value. Surprisal probabilities
    smaller than 1e-6 are returned as 1e-6.

2.  Using a Generalized Pareto Distribution fitted to the most extreme
    surprisal values (those with probability less than
    `threshold_probability`). This option is used when
    `approximation = "gdp"`, and is the default. For surprisal
    probabilities greater than `threshold_probability`, the value of
    `threshold_probability` is returned. Under this option, the
    distribution is used for computing the surprisal values but not for
    determining their probabilities. Due to extreme value theory, the
    resulting probabilities should be relatively insensitive to the
    distribution used in computing the surprisal values.

3.  Using ranks (`approximation = "rank"`), the approximate probability
    is the proportion of observations with greater surprisal values.
    This is also insensitive to the distribution used in computing the
    surprisal values.

## References

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Chapter
6, <https://OTexts.com/weird/>.

## Author

Rob J Hyndman

## Examples

``` r
# A linear model (i.e., a conditional Gaussian distribution)
lm_of <- lm(waiting ~ duration, data = oldfaithful)
oldfaithful |>
  mutate(
    fscore = surprisals_prob(lm_of, approximation = "none"),
    prob = surprisals_prob(lm_of, loo = TRUE, approximation = "none"),
  ) |>
  ggplot(aes(
    x = duration, y = waiting,
    color = prob < 0.01
  )) +
  geom_point()
#> Error in mutate(oldfaithful, fscore = surprisals_prob(lm_of, approximation = "none"),     prob = surprisals_prob(lm_of, loo = TRUE, approximation = "none"),     ): ℹ In argument: `prob = surprisals_prob(lm_of, loo = TRUE, approximation
#>   = "none")`.
#> Caused by error in `inherits()`:
#> ! argument "what" is missing, with no default
# A Poisson GLM
glm_breaks <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)
warpbreaks |>
  mutate(prob = surprisals_prob(glm_breaks, approximation = "none")) |>
  filter(prob < 0.05)
#>   breaks wool tension        prob
#> 1     70    A       L 0.006519066
#> 2     67    A       L 0.014393443
```
