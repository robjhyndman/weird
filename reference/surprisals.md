# Surprisals

Compute surprisals or surprisal probabilities from a model or a data
set. A surprisal is given by \\s = -\log f(y)\\ where \\f\\ is the
density or probability mass function of the estimated or assumed
distribution, and \\y\\ is an observation. A surprisal probability is
the probability of a surprisal at least as extreme as \\s\\.

The surprisal probabilities may be computed in three different ways.

1.  Given the same distribution that was used to compute the surprisal
    values. Under this option, surprisal probabilities are equal to 1
    minus the coverage probability of the largest HDR that contains each
    value. Surprisal probabilities smaller than 1e-6 are returned as
    1e-6.

2.  Using a Generalized Pareto Distribution fitted to the most extreme
    surprisal values (those with probability less than
    `threshold_probability`). This option is used if
    `approximation = "gpd"`. For surprisal probabilities greater than
    `threshold_probability`, the value of `threshold_probability` is
    returned. Under this option, the distribution is used for computing
    the surprisal values but not for determining their probabilities.
    Due to extreme value theory, the resulting probabilities should be
    relatively insensitive to the distribution used in computing the
    surprisal values.

3.  Empirically as the proportion of observations with greater surprisal
    values. This option is used when `approxiation = "empirical"`. This
    is also insensitive to the distribution used in computing the
    surprisal values.

## Usage

``` r
surprisals(
  object,
  probability = TRUE,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.1,
  ...
)

# Default S3 method
surprisals(
  object,
  probability = TRUE,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, multiplier = 2, ...),
  loo = FALSE,
  ...
)
```

## Arguments

- object:

  A model or numerical data set

- probability:

  Should surprisal probabilities be computed, or the surprisal values?

- approximation:

  Character string specifying the approximation to use in computing the
  surprisal probabilities. Ignored if `probability = FALSE`. : `none`
  specifies that no approximation is to be used; `gpd` specifies that
  the Generalized Pareto distribution should be used; while `empirical`
  specifies that the probabilities should be estimated empirically.

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"`).

- ...:

  Other arguments are passed to the appropriate method.

- distribution:

  A distribution object. If not provided, a kernel density estimate is
  computed from the data `object`.

- loo:

  Should leave-one-out surprisals be computed?

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

If no distribution is provided, a kernel density estimate is computed.
The leave-one-out surprisals (or LOO surprisals) are obtained by
estimating the kernel density estimate using all other observations.

## See also

[`dist_kde`](https://pkg.robjhyndman.com/weird-package/reference/dist_kde.md)

## Author

Rob J Hyndman

## Examples

``` r
# surprisals computed from bivariate data set
oldfaithful |>
  filter(duration < 7000, waiting < 7000) |>
  mutate(
    loo_fscores = surprisals(cbind(duration, waiting), loo = TRUE)
  )
#> # A tibble: 2,087 × 5
#>    time                recorded_duration duration waiting loo_fscores
#>    <dttm>              <chr>                <dbl>   <dbl>       <dbl>
#>  1 2017-01-14 00:06:00 3m 16s                 196    5940      0.0747
#>  2 2017-01-26 14:27:00 ~4m                    240    5820      0.894 
#>  3 2017-01-27 23:57:00 2m 1s                  121    3900      0.238 
#>  4 2017-01-30 15:09:00 ~4m                    240    5280      0.591 
#>  5 2017-01-31 13:27:00 ~3.5m                  210    5580      0.218 
#>  6 2017-01-31 15:00:00 ~4m                    240    5760      0.978 
#>  7 2017-02-03 23:13:00 3m 25s                 205    5160      0.126 
#>  8 2017-02-04 22:14:00 3m 34s                 214    5400      0.281 
#>  9 2017-02-05 17:19:00 4m 0s                  240    6060      0.762 
#> 10 2017-02-05 19:00:00 4m 2s                  242    6060      0.794 
#> # ℹ 2,077 more rows
# Univariate data
tibble(
  y = c(5, rnorm(49)),
  p_kde = surprisals(y, loo = TRUE),
  p_normal = surprisals(y, distribution = dist_normal()),
  p_zscore = 2 * (1 - pnorm(abs(y)))
)
#> # A tibble: 50 × 4
#>          y    p_kde    p_normal    p_zscore
#>      <dbl>    <dbl>       <dbl>       <dbl>
#>  1  5      0.000966 0.000000573 0.000000573
#>  2 -1.10   0.541    0.271       0.271      
#>  3  0.0635 0.858    0.949       0.949      
#>  4  0.584  0.642    0.560       0.560      
#>  5 -0.513  0.791    0.608       0.608      
#>  6 -0.813  0.663    0.416       0.416      
#>  7 -1.63   0.349    0.103       0.103      
#>  8  0.569  0.649    0.569       0.569      
#>  9  0.334  0.750    0.739       0.739      
#> 10 -0.211  0.892    0.833       0.833      
#> # ℹ 40 more rows
tibble(
  y = n01$v1,
  prob1 = surprisals(y, loo = TRUE),
  prob2 = surprisals(y, approximation = "gpd"),
  prob3 = surprisals(y, distribution = dist_normal()),
  prob4 = surprisals(y, distribution = dist_normal(), approximation = "gpd")
) |>
  arrange(prob1)
#> # A tibble: 1,000 × 5
#>        y    prob1    prob2    prob3    prob4
#>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1  3.81 0.000675 0.000113 0.000139 0.000135
#>  2  3.06 0.00694  0.00194  0.00225  0.00261 
#>  3 -3.01 0.0109   0.00362  0.00263  0.00308 
#>  4 -3.00 0.0112   0.00376  0.00273  0.00320 
#>  5 -2.94 0.0129   0.00456  0.00328  0.00389 
#>  6 -2.89 0.0146   0.00538  0.00387  0.00461 
#>  7  2.68 0.0199   0.00810  0.00746  0.00913 
#>  8  2.65 0.0214   0.00887  0.00807  0.00991 
#>  9 -2.60 0.0283   0.0128   0.00943  0.0116  
#> 10 -2.59 0.0285   0.0130   0.00953  0.0118  
#> # ℹ 990 more rows
# Bivariate data
tibble(
  x = rnorm(50),
  y = c(5, rnorm(49)),
  prob = surprisals(cbind(x, y)),
  lookout = lookout_prob(cbind(x, y))
)
#> # A tibble: 50 × 4
#>         x        y  prob  lookout
#>     <dbl>    <dbl> <dbl>    <dbl>
#>  1 -0.120  5        0.02 0.000782
#>  2 -1.52  -0.651    0.28 0.1     
#>  3 -1.51  -2.44     0.06 0.0543  
#>  4  1.40   0.997    0.32 0.1     
#>  5 -0.708  0.00511  0.76 0.1     
#>  6  0.828  0.791    0.56 0.1     
#>  7  1.71  -0.563    0.52 0.1     
#>  8 -0.779  0.214    0.74 0.1     
#>  9 -0.707  0.977    0.58 0.1     
#> 10 -1.99  -0.234    0.24 0.1     
#> # ℹ 40 more rows
```
