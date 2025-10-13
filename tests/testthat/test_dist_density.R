test_that("dist_density", {
  dist <- dist_density(
    list(
      1:2,
      seq(-4, 4, l = 999),
      seq(0, 10, l = 999)
    ),
    list(
      dnorm(1:2),
      dnorm(seq(-4, 4, l = 999)),
      dexp(seq(0, 10, l = 999))
    )
  )
  # Mean
  expect_equal(mean(dist), c(1.394, 0, 1), tolerance = 0.01)
  # Median
  expect_equal(unname(median(dist)), c(1.35, 0, log(2)), tolerance = 0.01)
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_equal(
    distributional::variance(dist),
    c(0.0721, 1, 1),
    tolerance = 0.01
  )
  # Density
  at <- seq(-4, 10, by = 1)
  expect_equal(
    density(dist, at),
    list(
      dnorm(at) * (at %in% c(1, 2)) / integral(1:2, dnorm(1:2)),
      dnorm(at),
      dexp(at)
    ),
    tolerance = 0.01
  )
  # CDF
  expect_equal(
    distributional::cdf(dist, at),
    list(as.integer(!(at < 2)), pnorm(at), pexp(at)),
    tolerance = 0.001
  )
  # Quantiles
  p <- (1:19) / 20
  expect_equal(
    quantile(dist, p = p),
    list(
      c(
        1.030950,
        1.062683,
        1.095260,
        1.128752,
        1.163242,
        1.198825,
        1.235611,
        1.273731,
        1.313342,
        1.354634,
        1.397841,
        1.443258,
        1.491261,
        1.542352,
        1.597216,
        1.656836,
        1.722719,
        1.797384,
        1.885701
      ),
      qnorm(p),
      qexp(p)
    ),
    tolerance = 0.01
  )
  # Generate
  set.seed(123)
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(
    lapply(rand_dist, mean) |> unlist(),
    mean(dist),
    tolerance = 0.01
  )
  expect_equal(
    lapply(rand_dist, var) |> unlist(),
    distributional::variance(dist),
    tolerance = 0.01
  )
  expect_equal(
    lapply(rand_dist, median) |> unlist(),
    unname(median(dist)),
    tolerance = 0.01
  )
})
