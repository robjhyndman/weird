# Univariate kde tests
test_that("dist_kde2", {
  set.seed(123)
  x <- rnorm(200)
  y <- c(rnorm(100), rnorm(100, 5))
  dist <- dist_kde(cbind(x, y))
  # Mean
  expect_identical(mean(dist), matrix(c(x = mean(x), y = mean(y)), nrow = 1))
  # Median
  expect_error(median(dist))
  # Variance
  expect_error(distributional::covariance(dist))
  # Density
  at <- expand.grid(x = seq(-3, 3, by = 0.5), y = seq(-2, 10, by = 2)) |>
    as.matrix()
  expect_true(all(density(dist, at)[[1]] >= 0))
  # CDF
  expect_error(distributional::cdf(dist, at))
  # Quantiles
  p <- (1:19) / 20
  expect_error(quantile(dist, p))
  # Generate
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(
    lapply(rand_dist, colMeans) |> unlist() |> matrix(nrow = 1),
    mean(dist),
    tolerance = 0.005
  )
})
