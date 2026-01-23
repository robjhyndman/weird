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
  surprisal probabilities. See Details below.

- threshold_probability:

  Probability threshold when computing the GPD approximation. This is
  the probability below which the GPD is fitted. Only used if
  `approximation = "gpd"`.

## Value

A numerical vector containing the surprisals or surprisal probabilities.

## Details

The surprisal probabilities may be computed in three different ways.

1.  When `approximation = "none"`, the surprisal probabilities are
    computed using the same distribution that was used to compute the
    surprisal values. Under this option, surprisal probabilities are
    equal to 1 minus the coverage probability of the largest HDR that
    contains each value. Surprisal probabilities smaller than 1e-6 are
    returned as 1e-6.

2.  When `approximation = "gdp"` (the default), the surprisal
    probabilities are computed using a Generalized Pareto Distribution
    fitted to the most extreme surprisal values (those with probability
    less than `threshold_probability`). For surprisal probabilities
    greater than `threshold_probability`, the value of
    `threshold_probability` is returned. Under this option, the
    distribution is used for computing the surprisal values but not for
    determining their probabilities. Due to extreme value theory, the
    resulting probabilities should be relatively insensitive to the
    distribution used in computing the surprisal values.

3.  When `approximation = "rank"`, the surprisal probability of each
    observation is estimated using the proportion of observations with
    greater surprisal values; i.e., 1 - rank(s)/n where `rank(s)` is the
    rank of the surprisal value `s` among all surprisal values, and `n`
    is the number of observations. This is a nonparametric approach that
    is also insensitive to the distribution used in computing the
    surprisal values.

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
#>  2  0.850   0.1  0.1     0.395      
#>  3 -0.925   0.1  0.1     0.355      
#>  4  0.894   0.1  0.1     0.372      
#>  5 -0.941   0.1  0.1     0.347      
#>  6  0.539   0.1  0.1     0.590      
#>  7 -0.182   0.1  0.1     0.856      
#>  8  0.892   0.1  0.1     0.373      
#>  9  1.33    0.1  0.1     0.184      
#> 10 -0.103   0.1  0.1     0.918      
#> # ℹ 40 more rows
tibble(
  y = n01$v1,
  prob1 = surprisals_prob(y),
  prob2 = surprisals_prob(y, loo = TRUE),
  prob3 = surprisals_prob(y, distribution = dist_normal(), approximation = "none"),
  prob4 = surprisals_prob(y, distribution = dist_normal(), approximation = "gpd")
) |>
  arrange(prob1)
#> # A tibble: 1,000 × 5
#>        y    prob1     prob2    prob3    prob4
#>    <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
#>  1  3.81 0.000261 0.0000746 0.000139 0.000135
#>  2  3.06 0.000850 0.00124   0.00225  0.00261 
#>  3 -3.01 0.00269  0.00361   0.00263  0.00308 
#>  4 -3.00 0.00282  0.00376   0.00273  0.00320 
#>  5 -2.94 0.00354  0.00457   0.00328  0.00389 
#>  6 -2.89 0.00432  0.00540   0.00387  0.00461 
#>  7  2.68 0.00625  0.00741   0.00746  0.00913 
#>  8  2.65 0.00713  0.00829   0.00807  0.00991 
#>  9 -2.60 0.0132   0.0142    0.00943  0.0116  
#> 10 -2.59 0.0134   0.0143    0.00953  0.0118  
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
oldfaithful |>
  mutate(
    s = surprisals(cbind(duration, waiting), loo = TRUE),
    p = surprisals_prob(cbind(duration, waiting), loo = TRUE)
  ) |>
  arrange(p)
#> # A tibble: 2,097 × 6
#>    time                recorded_duration       duration waiting     s         p
#>    <dttm>              <chr>                      <dbl>   <dbl> <dbl>     <dbl>
#>  1 2018-04-25 19:08:00 1s                             1    5700 Inf   0        
#>  2 2020-06-01 21:04:00 2 minutes                    120    6060 Inf   0        
#>  3 2022-12-03 16:20:00 ~4m                          240    3060 Inf   0        
#>  4 2022-12-07 17:19:00 ~4 30s                        30    5220 Inf   0        
#>  5 2023-07-04 12:03:00 ~1 minute 55ish seconds       60    4920 Inf   0        
#>  6 2020-09-04 01:38:00 >1m 50s                      110    6240  22.7 0.0000499
#>  7 2018-09-29 22:10:00 ~4m01s                       241    4500  18.4 0.00113  
#>  8 2022-09-08 05:13:00 5 min                        300    5280  18.3 0.00119  
#>  9 2021-01-22 18:35:00 2m50s                        170    3600  17.6 0.00210  
#> 10 2018-07-18 14:00:00 ~1m46s                       106    3000  17.6 0.00220  
#> # ℹ 2,087 more rows
```
