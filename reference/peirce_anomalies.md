# Anomalies according to Peirce's and Chauvenet's criteria

Peirce's criterion and Chauvenet's criterion were both proposed in the
1800s as a way of determining what observations should be rejected in a
univariate sample.

## Usage

``` r
peirce_anomalies(y)

chauvenet_anomalies(y)
```

## Arguments

- y:

  numerical vector of observations

## Value

A logical vector

## Details

These functions take a univariate sample `y` and return a logical vector
indicating which observations should be considered anomalies according
to either Peirce's criterion or Chauvenet's criterion.

## References

Peirce, B. (1852). Criterion for the rejection of doubtful observations.
*The Astronomical Journal*, 2(21), 161–163.

Chauvenet, W. (1863). 'Method of least squares'. Appendix to *Manual of
Spherical and Practical Astronomy*, Vol.2, Lippincott, Philadelphia,
pp.469-566.

## Author

Rob J Hyndman

## Examples

``` r
y <- rnorm(1000)
tibble(y = y) |> filter(peirce_anomalies(y))
#> # A tibble: 1 × 1
#>       y
#>   <dbl>
#> 1  3.81
tibble(y = y) |> filter(chauvenet_anomalies(y))
#> # A tibble: 1 × 1
#>       y
#>   <dbl>
#> 1  3.81
```
