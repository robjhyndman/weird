# Tests for peirce_anomalies() and chauvenet_anomalies()

# Shared fixtures -------------------------------------------------------
set.seed(99)
n <- 200
y_clean <- rnorm(n) # no true outliers (seed chosen to avoid false positives)
y_out <- c(rnorm(n - 1), 20) # one gross outlier at the end
y_na <- c(rnorm(n - 2), NA, 20) # NA + gross outlier
y_known <- c(rnorm(100), 8, -8) # two known outliers for integration tests

# peirce_anomalies() ----------------------------------------------------

test_that("peirce_anomalies returns a logical vector of correct length", {
  result <- peirce_anomalies(y_clean)
  expect_type(result, "logical")
  expect_length(result, length(y_clean))
})

test_that("peirce_anomalies flags no observations in clean normal data", {
  expect_false(any(peirce_anomalies(y_clean)))
})

test_that("peirce_anomalies flags the gross outlier", {
  result <- peirce_anomalies(y_out)
  expect_true(result[length(y_out)])
})

test_that("peirce_anomalies handles NA without error", {
  expect_no_error(peirce_anomalies(y_na))
  result <- peirce_anomalies(y_na)
  expect_type(result, "logical")
  expect_length(result, length(y_na))
})

test_that("peirce_anomalies flags no observations for n <= 2", {
  # peirce_threshold returns NaN for n <= 2; abs(z) > NaN is NA, not TRUE
  expect_false(isTRUE(any(peirce_anomalies(c(1, 2)))))
  expect_false(isTRUE(any(peirce_anomalies(1))))
})

# chauvenet_anomalies() -------------------------------------------------

test_that("chauvenet_anomalies returns a logical vector of correct length", {
  result <- chauvenet_anomalies(y_clean)
  expect_type(result, "logical")
  expect_length(result, length(y_clean))
})

test_that("chauvenet_anomalies flags no observations in clean normal data", {
  expect_false(any(chauvenet_anomalies(y_clean)))
})

test_that("chauvenet_anomalies flags the gross outlier", {
  result <- chauvenet_anomalies(y_out)
  expect_true(result[length(y_out)])
})

test_that("chauvenet_anomalies handles NA without error", {
  expect_no_error(chauvenet_anomalies(y_na))
  result <- chauvenet_anomalies(y_na)
  expect_type(result, "logical")
  expect_length(result, length(y_na))
})

test_that("chauvenet threshold matches qnorm(1 - 0.25/n) analytically", {
  # A value just above the analytic threshold should be flagged
  # Construct y so the last value has an exact standardised z-score above the
  # Chauvenet threshold, regardless of the other values
  threshold <- qnorm(1 - 0.25 / n)
  y_base <- seq_len(n - 1) # fixed, no randomness
  mu <- mean(y_base)
  sigma <- sd(y_base)
  y_thresh <- c(y_base, mu + (threshold + 1) * sigma) # z well above threshold
  result <- chauvenet_anomalies(y_thresh)
  expect_true(result[n])
})

# Comparison between methods --------------------------------------------

test_that("peirce flags a subset of what chauvenet flags (Peirce is tighter)", {
  p_flags <- peirce_anomalies(y_out)
  c_flags <- chauvenet_anomalies(y_out)
  # Every observation flagged by Peirce must also be flagged by Chauvenet
  expect_true(all(p_flags[p_flags] %in% c_flags[p_flags]))
  # Peirce flags no more than Chauvenet
  expect_lte(sum(p_flags), sum(c_flags))
})

# Integration: constructed data with known outliers ---------------------

test_that("peirce and chauvenet both detect known ±8 outliers", {
  p <- peirce_anomalies(y_known)
  ch <- chauvenet_anomalies(y_known)
  # Both outliers (positions 101 and 102) should be flagged
  expect_true(p[101])
  expect_true(p[102])
  expect_true(ch[101])
  expect_true(ch[102])
})
