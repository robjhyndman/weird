# Tests for surprisals() and surprisals_prob()

# Shared fixtures -------------------------------------------------------
set.seed(2)
y_uni <- rnorm(100)
y_out <- c(rnorm(99), 100) # one gross outlier at position 100
mat_bi <- cbind(rnorm(100), rnorm(100))
mat_out <- rbind(matrix(rnorm(198), ncol = 2), c(10, 10)) # outlier at row 99
df_bi <- as.data.frame(mat_bi)

dist_norm <- distributional::dist_normal()

# surprisals() ----------------------------------------------------------

test_that("surprisals.numeric returns a numeric vector of correct length", {
  set.seed(2)
  y <- c(rnorm(10), 100)
  expect_gt(max(surprisals(y, dist_normal())), 5000)
  expect_identical(max(surprisals(y, h = 1, loo = TRUE)), Inf)
  expect_equal(
    max(surprisals(y, h = 1, loo = FALSE)),
    log(11 / dnorm(0, 0, 1)),
    tolerance = 0.01
  )
})

test_that("surprisals returns a numeric vector of correct length (univariate)", {
  s <- surprisals(y_uni)
  expect_type(s, "double")
  expect_length(s, length(y_uni))
})

test_that("surprisals are non-negative", {
  s <- surprisals(y_uni)
  expect_true(all(s >= 0))
})

test_that("surprisals gross outlier has highest surprisal (univariate)", {
  s <- surprisals(y_out)
  expect_equal(which.max(s), length(y_out))
})

test_that("surprisals with explicit dist_normal gives finite values", {
  s <- surprisals(y_uni, distribution = dist_norm)
  expect_true(all(is.finite(s)))
  expect_length(s, length(y_uni))
})

test_that("surprisals loo = TRUE returns Inf for a gross outlier", {
  s <- surprisals(y_out, loo = TRUE)
  expect_equal(s[length(y_out)], Inf)
})

test_that("surprisals loo = TRUE differs from loo = FALSE", {
  s_loo <- surprisals(y_uni, loo = TRUE)
  s_nloo <- surprisals(y_uni, loo = FALSE)
  expect_false(isTRUE(all.equal(s_loo, s_nloo)))
})

test_that("surprisals.matrix returns correct length", {
  s <- surprisals(mat_bi)
  expect_type(s, "double")
  expect_length(s, nrow(mat_bi))
})

test_that("surprisals.matrix outlier row has highest surprisal", {
  s <- surprisals(mat_out)
  expect_equal(which.max(s), nrow(mat_out))
})

test_that("surprisals.matrix errors on non-numeric matrix", {
  m <- matrix(as.character(mat_bi), nrow = nrow(mat_bi))
  expect_error(surprisals(m), "numeric")
})

test_that("surprisals.data.frame returns correct length", {
  s <- surprisals(df_bi)
  expect_type(s, "double")
  expect_length(s, nrow(df_bi))
})

test_that("surprisals.data.frame agrees with surprisals.matrix", {
  s_df <- surprisals(df_bi)
  s_mat <- surprisals(mat_bi)
  expect_equal(s_df, s_mat)
})

# surprisals_prob() -----------------------------------------------------

test_that("surprisals_prob returns a numeric vector of correct length (univariate)", {
  p <- surprisals_prob(y_uni)
  expect_type(p, "double")
  expect_length(p, length(y_uni))
})

test_that("surprisals_prob values are in [0, 1]", {
  p <- surprisals_prob(y_uni)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob gross outlier gets lowest probability (univariate)", {
  p <- surprisals_prob(y_out)
  expect_equal(which.min(p), length(y_out))
})

test_that("surprisals_prob approximation = 'gpd' works (univariate)", {
  p <- surprisals_prob(y_uni, approximation = "gpd")
  expect_type(p, "double")
  expect_length(p, length(y_uni))
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob approximation = 'empirical' works (univariate)", {
  p <- surprisals_prob(y_uni, approximation = "empirical")
  expect_type(p, "double")
  expect_length(p, length(y_uni))
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob approximation = 'rank' agrees with 'empirical'", {
  p_rank <- surprisals_prob(y_uni, approximation = "rank")
  p_emp <- surprisals_prob(y_uni, approximation = "empirical")
  expect_equal(p_rank, p_emp)
})

test_that("surprisals_prob with dist_normal gives finite values", {
  p <- surprisals_prob(y_uni, distribution = dist_norm)
  expect_true(all(is.finite(p)))
})

test_that("surprisals_prob loo = TRUE works and differs from loo = FALSE", {
  p_loo <- surprisals_prob(y_uni, loo = TRUE)
  p_nloo <- surprisals_prob(y_uni, loo = FALSE)
  expect_length(p_loo, length(y_uni))
  expect_false(isTRUE(all.equal(p_loo, p_nloo)))
})

test_that("surprisals_prob.matrix works for bivariate data", {
  p <- surprisals_prob(mat_bi, approximation = "gpd")
  expect_type(p, "double")
  expect_length(p, nrow(mat_bi))
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.matrix outlier row gets lowest probability", {
  p <- surprisals_prob(mat_out, approximation = "gpd")
  expect_equal(which.min(p), nrow(mat_out))
})

test_that("surprisals_prob.matrix errors on non-numeric matrix", {
  m <- matrix(as.character(mat_bi), nrow = nrow(mat_bi))
  expect_error(surprisals_prob(m), "numeric")
})

test_that("surprisals_prob.data.frame agrees with surprisals_prob.matrix", {
  p_df <- surprisals_prob(df_bi, approximation = "gpd")
  p_mat <- surprisals_prob(mat_bi, approximation = "gpd")
  expect_equal(p_df, p_mat)
})
