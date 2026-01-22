# Surprisals and surprisal probabilities

A surprisal is given by \\s = -\log f(y)\\ where \\f\\ is the density or
probability mass function of the estimated or assumed distribution, and
\\y\\ is an observation. This is returned by
[`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md).
A surprisal probability is the probability of a surprisal at least as
extreme as \\s\\. This is returned by
[`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)

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
# Default S3 method
surprisals(object, distribution = dist_kde(object, ...), loo = FALSE, ...)

# Default S3 method
surprisals_prob(
  object,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)
```

## Arguments

- object:

  A model or numerical data set

- distribution:

  A distribution object. If not provided, a kernel density estimate is
  computed from the data `object`.

- loo:

  Should leave-one-out surprisals be computed?

- ...:

  Other arguments are passed to the appropriate method.

- approximation:

  Character string specifying the approximation to use in computing the
  surprisal probabilities. Ignored if `probability = FALSE`.
  `approximation = "none"` specifies that no approximation is to be
  used; `approximation = "gpd"` specifies that the Generalized Pareto
  distribution should be used; while `approximation = "empirical"`
  specifies that the probabilities should be estimated empirically.

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"` and `probability = TRUE`).

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

If no distribution is provided, a kernel density estimate is computed.
The leave-one-out surprisals (or LOO surprisals) are obtained by
estimating the kernel density estimate using all other observations.

## References

Rob J Hyndman (2026) "That's weird: Anomaly detection using R", Chapter
6, <https://OTexts.com/weird/>.

## See also

[`dist_kde`](https://pkg.robjhyndman.com/weird/reference/dist_kde.md)

## Author

Rob J Hyndman

## Examples

``` r
# Univariate data
tibble(
  y = c(5, rnorm(49)),
  p_kde = surprisals_prob(y, loo = TRUE),
  p_normal = surprisals_prob(y, distribution = dist_normal()),
  p_zscore = 2 * (1 - pnorm(abs(y)))
)
#> # A tibble: 50 × 4
#>         y p_kde    p_normal    p_zscore
#>     <dbl> <dbl>       <dbl>       <dbl>
#>  1  5     0     0.000000573 0.000000573
#>  2 -0.182 0.822 0.856       0.856      
#>  3  0.892 0.461 0.373       0.373      
#>  4  1.33  0.306 0.184       0.184      
#>  5 -0.103 0.862 0.918       0.918      
#>  6  0.615 0.588 0.539       0.539      
#>  7 -1.80  0.107 0.0719      0.0719     
#>  8 -0.263 0.772 0.793       0.793      
#>  9 -1.21  0.228 0.226       0.226      
#> 10  0.204 0.816 0.838       0.838      
#> # ℹ 40 more rows
tibble(
  y = n01$v1,
  prob1 = surprisals_prob(y, loo = TRUE),
  prob2 = surprisals_prob(y, approximation = "gpd"),
  prob3 = surprisals_prob(y, distribution = dist_normal()),
  prob4 = surprisals_prob(y, distribution = dist_normal(), approximation = "gpd")
) |>
  arrange(prob1)
#> # A tibble: 1,000 × 5
#>        y    prob1    prob2    prob3    prob4
#>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1  3.81 0.000230 0.000261 0.000139 0.000135
#>  2  3.06 0.00189  0.000850 0.00225  0.00261 
#>  3 -3.01 0.00427  0.00269  0.00263  0.00308 
#>  4 -3.00 0.00441  0.00282  0.00273  0.00320 
#>  5 -2.94 0.00529  0.00354  0.00328  0.00389 
#>  6 -2.89 0.00622  0.00432  0.00387  0.00461 
#>  7  2.68 0.00835  0.00625  0.00746  0.00913 
#>  8  2.65 0.00926  0.00713  0.00807  0.00991 
#>  9 -2.60 0.0152   0.0132   0.00943  0.0116  
#> 10 -2.59 0.0154   0.0134   0.00953  0.0118  
#> # ℹ 990 more rows
# Bivariate data
tibble(
  x = rnorm(50),
  y = c(5, rnorm(49)),
  prob = surprisals_prob(cbind(x, y))
)
#> Warning: Using an empirical approximation for multivariate data
#> # A tibble: 50 × 3
#>         x      y  prob
#>     <dbl>  <dbl> <dbl>
#>  1 -0.460  5      0.02
#>  2  0.238  0.408  0.9 
#>  3 -0.269  0.760  0.64
#>  4 -0.170 -2.29   0.12
#>  5  0.665  0.518  0.72
#>  6 -0.776 -1.35   0.24
#>  7 -1.92   0.363  0.28
#>  8  2.02   1.31   0.04
#>  9  0.435 -0.448  0.86
#> 10  1.05  -0.808  0.62
#> # ℹ 40 more rows
```
