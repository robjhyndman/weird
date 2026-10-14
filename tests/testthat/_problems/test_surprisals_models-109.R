# Extracted from test_surprisals_models.R:109

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "weird", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
lm_of <- lm(waiting ~ duration, data = oldfaithful)
glm_of <- glm(waiting ~ duration, data = oldfaithful)
glm_wb <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson)
glm_bin <- glm(
  cbind(breaks, 100 - breaks) ~ wool + tension,
  data = warpbreaks,
  family = binomial
)
glm_gamma <- glm(
  breaks ~ wool + tension,
  data = warpbreaks,
  family = Gamma
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

# test -------------------------------------------------------------------------
p <- surprisals_prob(glm_wb)
expect_type(p, "double")
expect_length(p, n_wb)
expect_true(all(p >= 0 & p <= 1, na.rm = TRUE))
