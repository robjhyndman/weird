# Univariate kde tests
test_that("dist_kde1", {
  set.seed(123)
  x <- rnorm(100)
  y <- c(rnorm(100), rnorm(100, 5))
  dist <- dist_kde(list(x, y))
  # Mean
  expect_identical(mean(dist), c(mean(x), mean(y)))
  # Median
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_identical(distributional::variance(dist) > 0, c(TRUE, TRUE))
  # Density
  at <- seq(-4, 10, by = 1)
  expect_identical(lengths(density(dist, at)), c(15L, 15L))
  # CDF
  expect_identical(lengths(distributional::cdf(dist, at)), c(15L, 15L))
  # Quantiles
  p <- (1:19) / 20
  expect_identical(lengths(quantile(dist, p = p)), c(19L, 19L))
  # Generate
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(
    lapply(rand_dist, mean) |> unlist(),
    mean(dist),
    tolerance = 0.005
  )
  expect_equal(
    lapply(rand_dist, var) |> unlist(),
    distributional::variance(dist),
    tolerance = 0.005
  )
})
