# Tests for surprisals() and surprisals_prob() with factors

# a: 5, b: 3, c: 1, d: 1, e: 0 (empty level)
y_fac <- factor(
  c(rep("a", 5), rep("b", 3), "c", "d"),
  levels = c("a", "b", "c", "d", "e")
)

test_that("surprisals.factor uses the sample proportions", {
  expect_equal(surprisals(y_fac), -log(c(rep(0.5, 5), rep(0.3, 3), 0.1, 0.1)))
})

test_that("surprisals_prob.factor sums probabilities of no more probable levels", {
  p <- surprisals_prob(y_fac)
  # Tied levels c and d each include both
  expect_equal(p, c(rep(1, 5), rep(0.5, 3), 0.2, 0.2))
})

test_that("surprisals_prob.factor matches a direct calculation", {
  set.seed(1)
  y <- factor(sample(letters[1:6], 200, replace = TRUE, prob = c(20, 10, 5, 3, 1, 1)))
  f <- c(table(y)) / length(y)
  expected <- sapply(f, \(x) sum(f[f <= x]))[as.character(y)]
  expect_equal(surprisals_prob(y), unname(expected))
})

test_that("missing values give missing surprisals", {
  y <- factor(c("a", NA, "a", "b"))
  expect_equal(surprisals(y), -log(c(2 / 3, NA, 2 / 3, 1 / 3)))
  expect_equal(surprisals_prob(y), c(1, NA, 1, 1 / 3))
})

test_that("prior adds a constant to each count", {
  # (n_k + 1) / (10 + 5)
  f <- c(6, 4, 2, 2, 1) / 15
  expect_equal(surprisals(y_fac, prior = 1), -log(f[as.integer(y_fac)]))
  expect_equal(surprisals_prob(y_fac, prior = 1)[9], sum(f[3:5]))
})

test_that("leave-one-out probabilities omit the observation", {
  s <- surprisals(y_fac, loo = TRUE)
  expect_equal(s[1], -log(4 / 9))
  expect_equal(s[6], -log(2 / 9))
  expect_identical(s[9], Inf)
  p <- surprisals_prob(y_fac, loo = TRUE)
  # Omitting one a: counts 4, 3, 1, 1 out of 9
  expect_equal(p[1], 1)
  # Omitting one b: counts 5, 2, 1, 1
  expect_equal(p[6], 4 / 9)
  # Omitting c: counts 5, 3, 0, 1; only c (and the empty e) is no more probable
  expect_equal(p[9], 0)
  # With a prior, singletons have finite surprisals
  expect_true(all(is.finite(surprisals(y_fac, loo = TRUE, prior = 0.5))))
})

test_that("prob can be given by name or by level order", {
  prob <- c(a = 0.1, b = 0.2, c = 0.3, d = 0.3, e = 0.1)
  expect_equal(surprisals(y_fac, prob = prob), -log(prob[as.integer(y_fac)]), ignore_attr = TRUE)
  expect_equal(
    surprisals_prob(y_fac, prob = rev(prob)),
    surprisals_prob(y_fac, prob = unname(prob))
  )
  # a and e are tied
  expect_equal(surprisals_prob(y_fac, prob = prob)[1], 0.2)
  expect_equal(surprisals_prob(y_fac, prob = prob)[9], 1)
})

test_that("tied probabilities with rounding error are treated as ties", {
  # 0.1 * 0.3 and 0.3 * 0.1 may differ in the last bit
  prob <- c(0.1 * 0.3, 0.3 * 0.1, 1 - 0.06)
  y <- factor(c("x", "y", "z"))
  expect_equal(surprisals_prob(y, prob = prob)[1:2], c(0.06, 0.06))
})

test_that("empirical approximation uses ranks of surprisals", {
  expect_equal(
    surprisals_prob(y_fac, approximation = "empirical"),
    c(rep(1, 5), rep(0.5, 3), 0.2, 0.2)
  )
  expect_identical(
    surprisals_prob(y_fac, approximation = "rank"),
    surprisals_prob(y_fac, approximation = "empirical")
  )
  expect_error(surprisals_prob(y_fac, approximation = "gpd"))
})

test_that("invalid arguments give errors", {
  expect_error(surprisals(y_fac, prob = c(0.5, 0.5)), "one value for each level")
  expect_error(surprisals(y_fac, prob = c(f = 0.2, b = 0.2, c = 0.2, d = 0.2, e = 0.2)), "names")
  expect_error(surprisals(y_fac, prob = rep(0.3, 5)), "sum to 1")
  expect_error(surprisals(y_fac, prob = rep(0.2, 5), loo = TRUE), "loo and prior")
  expect_error(surprisals(y_fac, prior = -1), "prior")
  expect_error(surprisals(factor(NA)), "no non-missing")
})
