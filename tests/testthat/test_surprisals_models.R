# Tests for surprisals() and surprisals_prob() for model objects

# Shared fixtures -------------------------------------------------------
lm_of <- lm(waiting ~ duration, data = oldfaithful)
glm_wb <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)
glm_bin <- glm(
  cbind(breaks, 100 - breaks) ~ wool + tension,
  data = warpbreaks,
  family = binomial
)
gam_gauss <- mgcv::gam(waiting ~ duration, data = oldfaithful)
gam_pois <- mgcv::gam(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = poisson
)
gam_bin <- mgcv::gam(
  cbind(breaks, 100 - breaks) ~ wool + tension,
  data = warpbreaks,
  family = binomial
)
gam_gamma <- mgcv::gam(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = Gamma
)

n_of <- nrow(oldfaithful)
n_wb <- nrow(warpbreaks)

# surprisals.lm ---------------------------------------------------------

test_that("surprisals.lm returns a finite numeric vector of correct length", {
  s <- surprisals(lm_of)
  expect_type(s, "double")
  expect_length(s, n_of)
  expect_true(all(is.finite(s)))
})

test_that("surprisals.lm values are non-negative", {
  expect_true(all(surprisals(lm_of) >= 0))
})

test_that("surprisals.lm loo = TRUE differs from loo = FALSE", {
  s <- surprisals(lm_of, loo = FALSE)
  s_loo <- surprisals(lm_of, loo = TRUE)
  expect_length(s_loo, n_of)
  expect_false(isTRUE(all.equal(s, s_loo)))
})

test_that("surprisals.lm loo = TRUE errors for a glm object", {
  expect_error(surprisals(glm_wb, loo = TRUE), "not implemented")
})

test_that("surprisals.lm works for a Poisson glm (via inheritance)", {
  s <- surprisals(glm_wb)
  expect_type(s, "double")
  expect_length(s, n_wb)
  expect_true(all(is.finite(s)))
  expect_true(all(s >= 0))
})

# surprisals_prob.lm ----------------------------------------------------

test_that("surprisals_prob.lm returns a numeric vector in [0, 1]", {
  p <- surprisals_prob(lm_of)
  expect_type(p, "double")
  expect_length(p, n_of)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.lm approximation = 'gpd' works", {
  p <- surprisals_prob(lm_of, approximation = "gpd")
  expect_length(p, n_of)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.lm approximation = 'empirical' works", {
  p <- surprisals_prob(lm_of, approximation = "empirical")
  expect_length(p, n_of)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.lm approximation = 'rank' agrees with 'empirical'", {
  p_rank <- surprisals_prob(lm_of, approximation = "rank")
  p_emp <- surprisals_prob(lm_of, approximation = "empirical")
  expect_equal(p_rank, p_emp)
})

test_that("surprisals_prob.lm loo = TRUE differs from loo = FALSE", {
  p <- surprisals_prob(lm_of, loo = FALSE)
  p_loo <- surprisals_prob(lm_of, loo = TRUE)
  expect_length(p_loo, n_of)
  expect_false(isTRUE(all.equal(p, p_loo)))
})

test_that("surprisals_prob.lm flags outliers in oldfaithful", {
  p <- surprisals_prob(lm_of)
  expect_gt(sum(p < 0.01, na.rm = TRUE), 0)
})

test_that("surprisals_prob.lm works for a Poisson glm (via inheritance)", {
  p <- surprisals_prob(glm_wb)
  expect_type(p, "double")
  expect_length(p, n_wb)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

# surprisals.gam --------------------------------------------------------

test_that("surprisals.gam Gaussian returns finite non-negative vector", {
  s <- surprisals(gam_gauss)
  expect_type(s, "double")
  expect_length(s, n_of)
  expect_true(all(is.finite(s)))
  expect_true(all(s >= 0))
})

test_that("surprisals.gam Poisson returns finite non-negative vector", {
  s <- surprisals(gam_pois)
  expect_type(s, "double")
  expect_length(s, n_wb)
  expect_true(all(is.finite(s)))
  expect_true(all(s >= 0))
})

test_that("surprisals.gam binomial returns finite non-negative vector", {
  s <- surprisals(gam_bin)
  expect_type(s, "double")
  expect_length(s, n_wb)
  expect_true(all(is.finite(s)))
  expect_true(all(s >= 0))
})

test_that("surprisals.gam errors on unsupported family", {
  expect_error(surprisals(gam_gamma), "Unsupported family")
})

# surprisals_prob.gam ---------------------------------------------------

test_that("surprisals_prob.gam Gaussian returns values in [0, 1]", {
  p <- surprisals_prob(gam_gauss)
  expect_type(p, "double")
  expect_length(p, n_of)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.gam Poisson returns values in [0, 1]", {
  p <- surprisals_prob(gam_pois)
  expect_type(p, "double")
  expect_length(p, n_wb)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.gam binomial returns values in [0, 1]", {
  p <- surprisals_prob(gam_bin)
  expect_type(p, "double")
  expect_length(p, n_wb)
  expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
})

test_that("surprisals_prob.gam errors on unsupported family", {
  expect_error(surprisals_prob(gam_gamma), "Unsupported family")
})

# Consistency between lm and gam ----------------------------------------
test_that("surprisals.lm and surprisals.gam Gaussian agree in order", {
  s_lm <- surprisals(lm_of)
  s_gam <- surprisals(gam_gauss)
  # Both should rank the same observations as most anomalous
  expect_equal(which.max(s_lm), which.max(s_gam))
})
