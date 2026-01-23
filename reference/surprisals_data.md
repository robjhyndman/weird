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
# S3 method for class 'numeric'
surprisals(object, distribution = dist_kde(object, ...), loo = FALSE, ...)

# S3 method for class 'matrix'
surprisals(object, distribution = dist_kde(object, ...), loo = FALSE, ...)

# S3 method for class 'data.frame'
surprisals(object, distribution = dist_kde(object, ...), loo = FALSE, ...)

# S3 method for class 'numeric'
surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)

# S3 method for class 'matrix'
surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)

# S3 method for class 'data.frame'
surprisals_prob(
  object,
  approximation = c("gpd", "rank", "none"),
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
#>         y p_kde p_normal    p_zscore
#>     <dbl> <dbl>    <dbl>       <dbl>
#>  1  5       0    0.00855 0.000000573
#>  2 -0.925   0.1  0.1     0.355      
#>  3  0.894   0.1  0.1     0.372      
#>  4 -0.941   0.1  0.1     0.347      
#>  5  0.539   0.1  0.1     0.590      
#>  6 -0.182   0.1  0.1     0.856      
#>  7  0.892   0.1  0.1     0.373      
#>  8  1.33    0.1  0.1     0.184      
#>  9 -0.103   0.1  0.1     0.918      
#> 10  0.615   0.1  0.1     0.539      
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
#>        y     prob1    prob2    prob3    prob4
#>    <dbl>     <dbl>    <dbl>    <dbl>    <dbl>
#>  1  3.81 0.0000746 0.000261 0.000135 0.000135
#>  2  3.06 0.00124   0.000850 0.00261  0.00261 
#>  3 -3.01 0.00361   0.00269  0.00308  0.00308 
#>  4 -3.00 0.00376   0.00282  0.00320  0.00320 
#>  5 -2.94 0.00457   0.00354  0.00389  0.00389 
#>  6 -2.89 0.00540   0.00432  0.00461  0.00461 
#>  7  2.68 0.00741   0.00625  0.00913  0.00913 
#>  8  2.65 0.00829   0.00713  0.00991  0.00991 
#>  9 -2.60 0.0142    0.0132   0.0116   0.0116  
#> 10 -2.59 0.0143    0.0134   0.0118   0.0118  
#> # ℹ 990 more rows
# Bivariate data
tibble(
  x = rnorm(50),
  y = c(5, rnorm(49)),
  prob = surprisals_prob(cbind(x, y))
)
#> # A tibble: 50 × 3
#>          x       y      prob
#>      <dbl>   <dbl>     <dbl>
#>  1 -0.341   5      0.0000488
#>  2  0.0496  0.344  0.1      
#>  3  1.30    0.0127 0.1      
#>  4  0.315  -0.873  0.1      
#>  5  0.802   0.343  0.1      
#>  6  0.486  -0.177  0.1      
#>  7  1.73    0.921  0.1      
#>  8  0.102   0.301  0.1      
#>  9 -0.990   0.693  0.1      
#> 10 -0.460   0.325  0.1      
#> # ℹ 40 more rows
```
