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
#>          y p_kde    p_normal    p_zscore
#>      <dbl> <dbl>       <dbl>       <dbl>
#>  1  5      0     0.000000573 0.000000573
#>  2  0.0635 0.774 0.949       0.949      
#>  3  0.584  0.558 0.560       0.560      
#>  4 -0.513  0.727 0.608       0.608      
#>  5 -0.813  0.569 0.416       0.416      
#>  6 -1.63   0.234 0.103       0.103      
#>  7  0.569  0.564 0.569       0.569      
#>  8  0.334  0.666 0.739       0.739      
#>  9 -0.211  0.823 0.833       0.833      
#> 10 -0.551  0.709 0.582       0.582      
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
  prob = surprisals(cbind(x, y)),
  lookout = lookout_prob(cbind(x, y))
)
#> # A tibble: 50 × 4
#>         x        y  prob lookout
#>     <dbl>    <dbl> <dbl>   <dbl>
#>  1 -1.52   5        4.90 0.00118
#>  2 -1.51  -2.44     4.43 0.0534 
#>  3  1.40   0.997    3.36 0.1    
#>  4 -0.708  0.00511  2.74 0.1    
#>  5  0.828  0.791    2.91 0.1    
#>  6  1.71  -0.563    2.82 0.1    
#>  7 -0.779  0.214    2.73 0.1    
#>  8 -0.707  0.977    2.79 0.1    
#>  9 -1.99  -0.234    3.65 0.1    
#> 10  1.90   0.849    3.72 0.1    
#> # ℹ 40 more rows
```
