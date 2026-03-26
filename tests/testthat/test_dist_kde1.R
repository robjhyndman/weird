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

test_that("dist_kde log_density", {
  set.seed(123)
  dist <- dist_kde(rnorm(100))
  at <- seq(-4, 4, by = 1)
  ld <- distributional:::log_density(dist, at)
  # log_density equals log(density)
  expect_equal(ld[[1]], log(density(dist, at)[[1]]))
  # Returns -Inf where density is zero
  expect_equal(distributional:::log_density(dist, -100)[[1]], -Inf)
  expect_true(is.numeric(distributional::skewness(dist)[[1]]))
  expect_true(is.numeric(distributional::kurtosis(dist)[[1]]))
  # Excess kurtosis of a large normal sample should be near 0
  set.seed(42)
  dist_large <- dist_kde(rnorm(10000))
  expect_equal(distributional::kurtosis(dist_large)[[1]], 0, tolerance = 0.1)
  fmt <- format(dist)
  expect_true(grepl("^kde\\[1d", fmt))
  expect_true(grepl("h=", fmt))
})

test_that("dist_kde hdr", {
  # Custom hdr method used when n >= 200
  set.seed(123)
  dist_large <- dist_kde(rnorm(200))
  h <- distributional::hdr(dist_large, size = 50)
  expect_s3_class(h, "hdr")
  # Fallback to default when n < 200
  dist_small <- dist_kde(rnorm(100))
  h_small <- distributional::hdr(dist_small, size = 50)
  expect_s3_class(h_small, "hdr")
  # The two should be relatively similar
  expect_equal(h, h_small, tolerance = 0.15)
})
