# Tests for lof_scores() and glosh_scores()

# Shared fixtures -------------------------------------------------------
set.seed(42)
y_uni   <- rnorm(100)
y_out   <- c(y_uni, 100)          # one gross outlier appended
y_na    <- c(y_uni[1:10], NA, y_uni[11:20])
y_mat   <- matrix(rnorm(200), ncol = 2)
y_mat_out <- rbind(y_mat, c(100, 100))

# lof_scores() ----------------------------------------------------------

test_that("lof_scores returns a numeric vector of length n", {
  scores <- lof_scores(y_uni)
  expect_type(scores, "double")
  expect_length(scores, length(y_uni))
})

test_that("lof_scores returns non-negative values with no Inf", {
  scores <- lof_scores(y_uni)
  expect_true(all(scores >= 0))
  expect_false(any(is.infinite(scores)))
})

test_that("lof_scores drops NAs before scoring", {
  scores <- lof_scores(y_na)
  expect_length(scores, sum(!is.na(y_na)))
  expect_false(anyNA(scores))
})

test_that("lof_scores assigns highest score to the gross outlier", {
  scores <- lof_scores(y_out)
  expect_equal(which.max(scores), length(y_out))
})

test_that("lof_scores works with a matrix input", {
  scores <- lof_scores(y_mat)
  expect_type(scores, "double")
  expect_length(scores, nrow(y_mat))
  expect_true(all(scores >= 0))
})

test_that("lof_scores assigns highest score to the outlier row in a matrix", {
  scores <- lof_scores(y_mat_out)
  expect_equal(which.max(scores), nrow(y_mat_out))
})

test_that("lof_scores respects the k argument", {
  scores_k5  <- lof_scores(y_uni, k = 5)
  scores_k20 <- lof_scores(y_uni, k = 20)
  expect_length(scores_k5,  length(y_uni))
  expect_length(scores_k20, length(y_uni))
  expect_false(isTRUE(all.equal(scores_k5, scores_k20)))
})

# glosh_scores() --------------------------------------------------------

test_that("glosh_scores returns a numeric vector of length n", {
  scores <- glosh_scores(y_uni)
  expect_type(scores, "double")
  expect_length(scores, length(y_uni))
})

test_that("glosh_scores values are in [0, 1]", {
  scores <- glosh_scores(y_uni)
  expect_true(all(scores >= 0))
  expect_true(all(scores <= 1))
})

test_that("glosh_scores assigns highest score to the gross outlier", {
  scores <- glosh_scores(y_out)
  expect_equal(which.max(scores), length(y_out))
})

test_that("glosh_scores works with a matrix input", {
  scores <- glosh_scores(y_mat)
  expect_type(scores, "double")
  expect_length(scores, nrow(y_mat))
  expect_true(all(scores >= 0))
  expect_true(all(scores <= 1))
})

test_that("glosh_scores assigns highest score to the outlier row in a matrix", {
  scores <- glosh_scores(y_mat_out)
  expect_equal(which.max(scores), nrow(y_mat_out))
})

test_that("glosh_scores respects the k argument", {
  scores_k5  <- glosh_scores(y_uni, k = 5)
  scores_k20 <- glosh_scores(y_uni, k = 20)
  expect_length(scores_k5,  length(y_uni))
  expect_length(scores_k20, length(y_uni))
  expect_false(isTRUE(all.equal(scores_k5, scores_k20)))
})
