# Statistical tests for anomalies using Grubbs' test and Dixon's test

Grubbs' test (proposed in 1950) identifies possible anomalies in
univariate data using z-scores assuming the data come from a normal
distribution. Dixon's test (also from 1950) compares the difference in
the largest two values to the range of the data. Critical values for
Dixon's test have been computed using simulation with interpolation
using a quadratic model on logit(alpha) and log(log(n)).

## Usage

``` r
grubbs_anomalies(y, alpha = 0.05)

dixon_anomalies(y, alpha = 0.05, two_sided = TRUE)
```

## Arguments

- y:

  numerical vector of observations

- alpha:

  size of the test.

- two_sided:

  If `TRUE`, both minimum and maximums will be considered. Otherwise
  only the maximum will be used. (Take negative values to consider only
  the minimum with `two_sided=FALSE`.)

## Value

A logical vector

## Details

Grubbs' test is based on z-scores, and a point is identified as an
anomaly when the associated absolute z-score is greater than a threshold
value. A vector of logical values is returned, where `TRUE` indicates an
anomaly. This version of Grubbs' test looks for outliers anywhere in the
sample. Grubbs' original test came in several variations which looked
for one outlier, or two outliers in one tail, or two outliers on
opposite tails. These variations are implemented in the `grubbs.test`
function. Dixon's test only considers the maximum (and possibly the
minimum) as potential outliers.

## References

Grubbs, F. E. (1950). Sample criteria for testing outlying observations.
*Annals of Mathematical Statistics*, 21(1), 27–58. Dixon, W. J. (1950).
Analysis of extreme values. *Annals of Mathematical Statistics*, 21(4),
488–506.

## See also

`grubbs.test`, `dixon.test`

## Author

Rob J Hyndman

## Examples

``` r
x <- c(rnorm(1000), 5:10)
tibble(x = x) |> filter(grubbs_anomalies(x))
#> # A tibble: 6 × 1
#>       x
#>   <dbl>
#> 1     5
#> 2     6
#> 3     7
#> 4     8
#> 5     9
#> 6    10
tibble(x = x) |> filter(dixon_anomalies(x))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: x <dbl>
y <- c(rnorm(1000), 5)
tibble(y = y) |> filter(grubbs_anomalies(y))
#> # A tibble: 1 × 1
#>       y
#>   <dbl>
#> 1     5
tibble(y = y) |> filter(dixon_anomalies(y))
#> # A tibble: 1 × 1
#>       y
#>   <dbl>
#> 1     5
```
