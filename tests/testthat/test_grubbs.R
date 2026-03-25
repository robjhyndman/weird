set.seed(1)
y_clean <- rnorm(100)
y_outlier <- c(rnorm(100), 10)
y_both <- c(-10, rnorm(100), 10)
df_outlier <- dplyr::tibble(x = y_outlier)

# --- grubbs_anomalies --------------------------------------------------------
grubbs_clean <- grubbs_anomalies(y_clean)
grubbs_outlier <- grubbs_anomalies(y_outlier)

test_that("grubbs_anomalies returns a logical vector of the same length as input", {
  expect_type(grubbs_clean, "logical")
  expect_length(grubbs_clean, length(y_clean))
})

test_that("grubbs_anomalies detects an obvious high outlier", {
  expect_true(grubbs_outlier[length(grubbs_outlier)]) # the appended 10 should be flagged
  expect_lt(sum(grubbs_outlier), 5) # but not too many flagged
})

test_that("grubbs_anomalies flags no anomalies in clean normal data (low FPR)", {
  # With n=100 and alpha=0.05 the familywise FPR should be very low after Bonferroni
  expect_lt(sum(grubbs_clean), 5)
})

test_that("grubbs_anomalies respects the alpha parameter", {
  # Stricter alpha should flag fewer or equal points
  result_loose <- grubbs_anomalies(y_outlier, alpha = 0.10)
  result_strict <- grubbs_anomalies(y_outlier, alpha = 0.001)
  expect_gte(sum(result_loose), sum(result_strict))
})

test_that("grubbs_anomalies handles NA values without error", {
  y_na <- c(y_clean, NA)
  expect_no_error(grubbs_anomalies(y_na))
})

test_that("grubbs_anomalies works with dplyr filter", {
  result <- df_outlier |> dplyr::filter(grubbs_anomalies(x))
  expect_true(nrow(result) >= 1)
  expect_true(10 %in% result$x)
})

# --- dixon_anomalies ----------------------------------------------------------

dixon_clean <- dixon_anomalies(y_clean)
dixon_outlier <- dixon_anomalies(y_outlier)
dixon_both_two <- dixon_anomalies(y_both, two_sided = TRUE)
dixon_both_one <- dixon_anomalies(y_both, two_sided = FALSE)

test_that("dixon_anomalies returns a logical vector of the same length as input", {
  expect_type(dixon_clean, "logical")
  expect_length(dixon_clean, length(y_clean))
})

test_that("dixon_anomalies detects an obvious high outlier", {
  expect_true(dixon_outlier[which(y_outlier == 10)])
})

test_that("dixon_anomalies with two_sided=TRUE can flag both tails", {
  expect_true(dixon_both_two[which(y_both == 10)])
  expect_true(dixon_both_two[which(y_both == -10)])
})

test_that("dixon_anomalies with two_sided=FALSE only considers the maximum", {
  # The minimum (-10) should NOT be flagged
  expect_false(dixon_both_one[which(y_both == -10)])
})

test_that("dixon_anomalies respects the alpha parameter", {
  result_loose <- dixon_anomalies(y_outlier, alpha = 0.10)
  result_strict <- dixon_anomalies(y_outlier, alpha = 0.001)
  expect_gte(sum(result_loose), sum(result_strict))
})

test_that("dixon_anomalies flags no more than 2 points (max + min)", {
  expect_equal(sum(dixon_both_two), 2)
})

test_that("dixon_anomalies works with dplyr filter", {
  result <- df_outlier |> dplyr::filter(dixon_anomalies(x))
  expect_true(nrow(result) == 1L)
  expect_equal(10, result$x)
})
