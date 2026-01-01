# Surprisals computed from a model

Surprisals computed from a model

## Usage

``` r
# S3 method for class 'lm'
surprisals(
  object,
  probability = TRUE,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.1,
  loo = FALSE,
  ...
)

# S3 method for class 'gam'
surprisals(
  object,
  probability = TRUE,
  approximation = c("none", "gpd", "empirical"),
  threshold_probability = 0.1,
  loo = FALSE,
  ...
)
```

## Arguments

- object:

  A model object such as returned by
  [`lm`](https://rdrr.io/r/stats/lm.html), or
  [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html).

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

- loo:

  Should leave-one-out surprisals be computed?

- ...:

  Other arguments are ignored.

## Examples

``` r
# surprisals computed from linear model
of <- oldfaithful |>
  filter(duration < 7200, waiting < 7200)
lm_of <- lm(waiting ~ duration, data = of)
of |>
  mutate(
    fscore = surprisals(lm_of),
    loo_fscore = surprisals(lm_of, loo = TRUE),
    # lookout_prob = lookout(surprisals = fscore, loo_scores = loo_fscore)
  ) |>
  ggplot(aes(
    x = duration, y = waiting,
    color = loo_fscore > quantile(loo_fscore, 0.99)
  )) +
  geom_point()

# surprisals computed from GAM
of <- oldfaithful |>
  filter(duration > 1, duration < 7200, waiting < 7200)
gam_of <- mgcv::gam(waiting ~ s(duration), data = of)
of |>
  mutate(fscore = surprisals(gam_of))
#> # A tibble: 2,096 × 5
#>    time                recorded_duration duration waiting fscore
#>    <dttm>              <chr>                <dbl>   <dbl>  <dbl>
#>  1 2017-01-14 00:06:00 3m 16s                 196    5940  0.719
#>  2 2017-01-26 14:27:00 ~4m                    240    5820  0.929
#>  3 2017-01-27 23:57:00 2m 1s                  121    3900  0.740
#>  4 2017-01-30 15:09:00 ~4m                    240    5280  0.109
#>  5 2017-01-31 13:27:00 ~3.5m                  210    5580  0.942
#>  6 2017-01-31 15:00:00 ~4m                    240    5760  0.921
#>  7 2017-02-03 23:13:00 3m 25s                 205    5160  0.579
#>  8 2017-02-04 22:14:00 3m 34s                 214    5400  0.724
#>  9 2017-02-05 17:19:00 4m 0s                  240    6060  0.401
#> 10 2017-02-05 19:00:00 4m 2s                  242    6060  0.429
#> # ℹ 2,086 more rows
```
