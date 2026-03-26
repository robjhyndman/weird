# Tests for kde_bandwidth()

# Shared fixtures -------------------------------------------------------
set.seed(123)
x_uni <- rnorm(100)
x_out <- c(x_uni, rep(100, 5)) # x_uni + gross outliers
dat_bi <- matrix(rnorm(200), ncol = 2)
dat_tri <- matrix(rnorm(300), ncol = 3)
df_bi <- as.data.frame(dat_bi)

# Helpers
is_pd <- function(m) all(eigen(m)$values > 0) # positive-definite check

# Univariate ------------------------------------------------------------

test_that("kde_bandwidth default method is 'robust'", {
  expect_equal(kde_bandwidth(x_uni), kde_bandwidth(x_uni, method = "robust"))
})

test_that("kde_bandwidth returns a positive scalar for univariate data", {
  h <- kde_bandwidth(x_uni)
  expect_length(h, 1)
  expect_true(is.numeric(h))
  expect_gt(h, 0)
})

test_that("kde_bandwidth normal/robust agree on clean univariate data", {
  h_robust <- kde_bandwidth(x_uni, method = "robust")
  h_normal <- kde_bandwidth(x_uni, method = "normal")
  expect_gt(h_normal, 0)
  expect_equal(h_normal, h_robust, tolerance = 0.1)
})

test_that("kde_bandwidth robust resists outliers; normal does not", {
  h_clean <- kde_bandwidth(x_uni, method = "robust")
  h_normal_out <- kde_bandwidth(x_out, method = "normal")
  h_robust_out <- kde_bandwidth(x_out, method = "robust")
  expect_lt(h_robust_out, h_normal_out)
  expect_equal(h_robust_out, h_clean, tolerance = 0.1)
})

test_that("kde_bandwidth plugin works for univariate data", {
  h <- kde_bandwidth(x_uni, method = "plugin")
  expect_length(h, 1)
  expect_gt(h, 0)
})

test_that("kde_bandwidth lookout works for univariate data", {
  h <- kde_bandwidth(x_uni, method = "lookout")
  expect_length(h, 1)
  expect_gt(h, 0)
})

# Multivariate ----------------------------------------------------------

test_that("kde_bandwidth returns a positive-definite 2x2 matrix (robust)", {
  h <- kde_bandwidth(dat_bi)
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(2L, 2L))
  expect_true(is_pd(h))
})

test_that("kde_bandwidth normal works for multivariate data", {
  h <- kde_bandwidth(dat_tri, method = "normal")
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(3L, 3L))
  expect_true(is_pd(h))
})

test_that("kde_bandwidth plugin works for bivariate data", {
  h <- kde_bandwidth(dat_bi, method = "plugin")
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(2L, 2L))
  expect_true(is_pd(h))
})

test_that("kde_bandwidth lookout works for bivariate data", {
  h <- kde_bandwidth(dat_bi, method = "lookout")
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(2L, 2L))
  expect_true(is_pd(h))
})

# Input types -----------------------------------------------------------

test_that("kde_bandwidth accepts a data frame", {
  h <- kde_bandwidth(df_bi)
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(2L, 2L))
})

# Integration -----------------------------------------------------------

test_that("kde_bandwidth works with oldfaithful data", {
  h1 <- kde_bandwidth(oldfaithful$duration)
  expect_length(h1, 1)
  expect_gt(h1, 0)

  h2 <- kde_bandwidth(oldfaithful[, c("duration", "waiting")])
  expect_true(is.matrix(h2))
  expect_equal(dim(h2), c(2L, 2L))
})
