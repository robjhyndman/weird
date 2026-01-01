# Lookout probabilities

The lookout algorithm (Kandanaarachchi & Hyndman, 2022) computes
leave-one-out surprisal probabilities from a kernel density estimate
using a Generalized Pareto distribution. The kernel density estimate
uses a bandwidth based on topological data analysis and a quadratic
kernel. So it is similar but not identical to using
[`surprisals`](https://pkg.robjhyndman.com/weird-package/reference/surprisals.md)
with `loo = TRUE` and `approximation = "gdp"`. A low probability
indicates a likely anomaly.

## Usage

``` r
lookout_prob(object, ...)
```

## Arguments

- object:

  A numerical data set.

- ...:

  Other arguments are passed to
  [`lookout`](https://sevvandi.github.io/lookout/reference/lookout.html).

## Value

A numerical vector containing the lookout probabilities

## References

Sevvandi Kandanaarachchi & Rob J Hyndman (2022) "Leave-one-out kernel
density estimates for outlier detection", *J Computational & Graphical
Statistics*, **31**(2), 586-599.
<https://robjhyndman.com/publications/lookout/>

## See also

[`lookout`](https://sevvandi.github.io/lookout/reference/lookout.html)

## Author

Rob J Hyndman

## Examples

``` r
# Univariate data
tibble(
  y = c(5, rnorm(49)),
  lookout = lookout_prob(y)
)
#> # A tibble: 50 × 2
#>          y lookout
#>      <dbl>   <dbl>
#>  1  5       0     
#>  2 -0.0506  0.1   
#>  3 -0.306   0.1   
#>  4  0.894   0.1   
#>  5 -1.05    0.1   
#>  6  1.97    0.0648
#>  7 -0.384   0.1   
#>  8  1.65    0.0960
#>  9  1.51    0.1   
#> 10  0.0830  0.1   
#> # ℹ 40 more rows
# Bivariate data
tibble(
  x = rnorm(50),
  y = c(5, rnorm(49)),
  lookout = lookout_prob(cbind(x, y))
)
#> # A tibble: 50 × 3
#>         x       y  lookout
#>     <dbl>   <dbl>    <dbl>
#>  1 -0.191  5      0.000693
#>  2  0.803  0.0450 0.1     
#>  3  1.89  -0.715  0.0994  
#>  4  1.47   0.865  0.1     
#>  5  0.677  1.07   0.1     
#>  6  0.380  1.90   0.1     
#>  7 -0.193 -0.603  0.1     
#>  8  1.58  -0.391  0.1     
#>  9  0.596 -0.416  0.1     
#> 10 -1.17  -0.376  0.1     
#> # ℹ 40 more rows
# Using a regression model
of <- oldfaithful |> filter(duration < 7200, waiting < 7200)
fit_of <- lm(waiting ~ duration, data = of)
broom::augment(fit_of) |>
  mutate(lookout = lookout_prob(.std.resid)) |>
  arrange(lookout)
#> # A tibble: 2,097 × 9
#>    waiting duration .fitted .resid     .hat .sigma .cooksd .std.resid  lookout
#>      <dbl>    <dbl>   <dbl>  <dbl>    <dbl>  <dbl>   <dbl>      <dbl>    <dbl>
#>  1    5700        1   2836.  2864. 0.0116     442. 0.245         6.46 0       
#>  2    3060      240   5776. -2716. 0.000517   442. 0.00961      -6.09 0       
#>  3    6240      110   4177.  2063. 0.00344    444. 0.0371        4.63 1.45e-11
#>  4    5220       30   3193.  2027. 0.00892    444. 0.0938        4.57 3.16e-11
#>  5    6060      120   4300.  1760. 0.00295    444. 0.0231        3.95 1.24e- 8
#>  6    6971      210   5407.  1564. 0.000536   445. 0.00330       3.51 3.05e- 7
#>  7    6780      195   5223.  1557. 0.000693   445. 0.00423       3.49 3.32e- 7
#>  8    7020      216   5481.  1539. 0.000501   445. 0.00298       3.45 4.24e- 7
#>  9    6900      208   5383.  1517. 0.000551   445. 0.00319       3.40 5.57e- 7
#> 10    6540      180   5038.  1502. 0.000948   445. 0.00539       3.37 6.71e- 7
#> # ℹ 2,087 more rows
```
