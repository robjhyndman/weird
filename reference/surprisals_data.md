# Surprisals and surprisal probabilities computed from data

A surprisal is given by \\s = -\log f(y)\\ where \\f\\ is the density or
probability mass function of the estimated or assumed distribution, and
\\y\\ is an observation. This is returned by
[`surprisals()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md).
A surprisal probability is the probability of a surprisal at least as
extreme as \\s\\. This is returned by
[`surprisals_prob()`](https://pkg.robjhyndman.com/weird/reference/surprisals.md)

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
  approximation = c("none", "gpd", "empirical", "rank"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)

# S3 method for class 'matrix'
surprisals_prob(
  object,
  approximation = c("none", "gpd", "empirical", "rank"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)

# S3 method for class 'data.frame'
surprisals_prob(
  object,
  approximation = c("none", "gpd", "empirical", "rank"),
  threshold_probability = 0.1,
  distribution = dist_kde(object, ...),
  loo = FALSE,
  ...
)
```

## Arguments

- object:

  A numerical data set (either a vector, matrix, or a data.frame
  containing only numerical columns).

- distribution:

  A distribution object. By default, a kernel density estimate is
  computed from the data `object`.

- loo:

  Should leave-one-out surprisals be computed?

- ...:

  Other arguments are passed to the appropriate method.

- approximation:

  Character string specifying the method to use in computing the
  surprisal probabilities. See Details below. For a multivariate data
  set, it needs to be set to either "gpd" or "empirical".

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"`.

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

The surprisal probabilities may be computed in three different ways.

1.  When `approximation = "none"` (the default), the surprisal
    probabilities are computed using the same distribution that was used
    to compute the surprisal values. Under this option, surprisal
    probabilities are equal to 1 minus the coverage probability of the
    largest HDR that contains each value. Surprisal probabilities
    smaller than 1e-6 are returned as 1e-6.

2.  When `approximation = "gdp"`, the surprisal probabilities are
    computed using a Generalized Pareto Distribution fitted to the most
    extreme surprisal values (those with probability less than
    `threshold_probability`). For surprisal probabilities greater than
    `threshold_probability`, the value of `threshold_probability` is
    returned. Under this option, the distribution is used for computing
    the surprisal values but not for determining their probabilities.
    Due to extreme value theory, the resulting probabilities should be
    relatively insensitive to the distribution used in computing the
    surprisal values.

3.  When `approximation = "empirical"` (or `"rank"`), the surprisal
    probability of each observation is estimated using the proportion of
    observations with greater or equal surprisal values. This is a
    nonparametric approach that is also insensitive to the distribution
    used in computing the surprisal values.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter
6, <https://OTexts.com/weird/>.

Hyndman, R J & Frazier, D T (2026) "Anomaly detection using surprisals",
<https://robjhyndman.com/publications/surprisals.html>.

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
#>  2  0.869  0.352 0.385       0.385      
#>  3 -1.62   0.181 0.104       0.104      
#>  4  0.143  0.678 0.886       0.886      
#>  5  0.710  0.409 0.477       0.477      
#>  6 -0.0555 0.790 0.956       0.956      
#>  7  0.172  0.662 0.864       0.864      
#>  8 -0.639  0.710 0.523       0.523      
#>  9 -0.663  0.695 0.508       0.508      
#> 10  0.0788 0.715 0.937       0.937      
#> # ℹ 40 more rows
tibble(
  y = n01$v1,
  prob1 = surprisals_prob(y),
  prob2 = surprisals_prob(y, loo = TRUE),
  prob3 = surprisals_prob(y, distribution = dist_normal()),
  prob4 = surprisals_prob(y, distribution = dist_normal(), approximation = "gpd")
) |>
  arrange(prob1)
#> # A tibble: 1,000 × 5
#>        y   prob1    prob2    prob3    prob4
#>    <dbl>   <dbl>    <dbl>    <dbl>    <dbl>
#>  1  3.81 0.00150 0.000229 0.000139 0.000135
#>  2  3.06 0.00258 0.00188  0.00225  0.00261 
#>  3 -3.01 0.00509 0.00425  0.00263  0.00308 
#>  4 -3.00 0.00527 0.00440  0.00273  0.00320 
#>  5 -2.94 0.00619 0.00528  0.00328  0.00389 
#>  6 -2.89 0.00711 0.00621  0.00387  0.00461 
#>  7  2.68 0.00925 0.00834  0.00746  0.00913 
#>  8  2.65 0.0102  0.00925  0.00807  0.00991 
#>  9 -2.60 0.0162  0.0152   0.00943  0.0116  
#> 10 -2.59 0.0164  0.0154   0.00953  0.0118  
#> # ℹ 990 more rows
# Bivariate data
tibble(
  x = rnorm(50),
  y = c(5, rnorm(49)),
  prob = surprisals_prob(cbind(x, y), approximation = "gpd")
)
#> # A tibble: 50 × 3
#>           x       y    prob
#>       <dbl>   <dbl>   <dbl>
#>  1  0.107    5      0.00194
#>  2 -1.01     0.818  0.1    
#>  3  1.18     0.519  0.1    
#>  4 -1.84    -0.614  0.1    
#>  5 -1.21     2.83   0.0301 
#>  6 -0.772    1.50   0.1    
#>  7 -0.350   -0.0382 0.1    
#>  8  1.68    -0.833  0.0999 
#>  9 -2.29     0.442  0.1    
#> 10 -0.00475 -0.362  0.1    
#> # ℹ 40 more rows
oldfaithful |>
  mutate(
    s = surprisals(cbind(duration, waiting), loo = TRUE),
    p = surprisals_prob(cbind(duration, waiting), loo = TRUE, approximation = "gpd")
  ) |>
  arrange(p)
#> # A tibble: 2,097 × 6
#>    time                recorded_duration        duration waiting     s        p
#>    <dttm>              <chr>                       <dbl>   <dbl> <dbl>    <dbl>
#>  1 2018-04-25 19:08:00 1s                              1    5700 Inf   0       
#>  2 2020-06-01 21:04:00 2 minutes                     120    6060 Inf   0       
#>  3 2020-09-04 01:38:00 >1m 50s                       110    6240 Inf   0       
#>  4 2022-12-03 16:20:00 ~4m                           240    3060 Inf   0       
#>  5 2022-12-07 17:19:00 ~4 30s                         30    5220 Inf   0       
#>  6 2023-07-04 12:03:00 ~1 minute 55ish seconds        60    4920 Inf   0       
#>  7 2023-07-23 21:57:00 ~3 minutes 40ish seconds      180    6540  18.2 0.000817
#>  8 2022-09-08 05:13:00 5 min                         300    5280  18.2 0.000853
#>  9 2021-01-22 18:35:00 2m50s                         170    3600  18.1 0.000878
#> 10 2018-09-29 22:10:00 ~4m01s                        241    4500  18.1 0.000895
#> # ℹ 2,087 more rows
```
