# Tests for stray_scores() and stray_anomalies()

# Shared fixtures -------------------------------------------------------
set.seed(1)
n       <- 50
y_clean <- rnorm(n)
y_out   <- c(rnorm(n - 1), 20)          # gross outlier at position n
mat_clean <- cbind(rnorm(n), rnorm(n))
mat_out   <- cbind(rnorm(n), c(10, rnorm(n - 1)))  # outlier at row 1

# stray_scores() --------------------------------------------------------

test_that("stray_scores returns a numeric vector of correct length", {
  scores <- stray_scores(y_clean)
  expect_type(scores, "double")
  expect_length(scores, n)
})

test_that("stray_scores are non-negative", {
  scores <- stray_scores(y_clean)
  expect_true(all(scores >= 0))
})

test_that("stray_scores assigns the highest score to the gross outlier", {
  scores <- stray_scores(y_out)
  expect_equal(which.max(scores), n)
})

test_that("stray_scores works with matrix input", {
  scores <- stray_scores(mat_clean)
  expect_type(scores, "double")
  expect_length(scores, n)
})

test_that("stray_scores assigns highest score to outlier row in matrix", {
  scores <- stray_scores(mat_out)
  expect_equal(which.max(scores), 1L)
})

# stray_anomalies() -----------------------------------------------------

test_that("stray_anomalies returns a logical vector of correct length", {
  result <- stray_anomalies(y_clean)
  expect_type(result, "logical")
  expect_length(result, n)
})

test_that("stray_anomalies flags the gross univariate outlier", {
  result <- stray_anomalies(y_out)
  expect_true(result[n])
})

test_that("stray_anomalies flags no observations in clean normal data", {
  result <- stray_anomalies(y_clean)
  expect_false(any(result))
})

test_that("stray_anomalies works with matrix input", {
  result <- stray_anomalies(mat_clean)
  expect_type(result, "logical")
  expect_length(result, n)
})

test_that("stray_anomalies flags the outlier row in a matrix", {
  result <- stray_anomalies(mat_out)
  expect_true(result[1L])
})

# Consistency between functions -----------------------------------------

test_that("stray_anomalies TRUE positions have higher scores than FALSE positions", {
  scores  <- stray_scores(y_out)
  flags   <- stray_anomalies(y_out)
  if (any(flags) && any(!flags)) {
    expect_gt(min(scores[flags]), max(scores[!flags]))
  }
})

test_that("stray_scores and stray_anomalies agree on number of outliers", {
  scores  <- stray_scores(y_out)
  flags   <- stray_anomalies(y_out)
  # The single outlier index reported by find_HDoutliers should be flagged
  out_idx <- stray::find_HDoutliers(data = y_out)$outliers
  expect_true(all(flags[out_idx]))
})
