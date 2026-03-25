set.seed(42)
y_clean <- rnorm(50)
y_spike <- c(rnorm(20), 20, rnorm(20)) # spike at position 21
y_two_spikes <- c(rnorm(10), 20, rnorm(18), -20, rnorm(10))

# Pre-computed results reused across tests
hampel_clean <- hampel_anomalies(y_clean, bandwidth = 3)
hampel_clean_k3 <- hampel_anomalies(y_clean, bandwidth = 3, k = 3)
hampel_spike <- hampel_anomalies(y_spike, bandwidth = 3, k = 3)
hampel_two_spikes <- hampel_anomalies(y_two_spikes, bandwidth = 3, k = 3)
df_spike <- dplyr::tibble(time = seq_along(y_spike), y = y_spike) |>
  dplyr::mutate(is_anomaly = hampel_anomalies(y, bandwidth = 3, k = 3))

# --- hampel_anomalies ---------------------------------------------------------

test_that("hampel_anomalies returns a logical vector of the same length as input", {
  expect_type(hampel_clean, "logical")
  expect_length(hampel_clean, length(y_clean))
})

test_that("hampel_anomalies detects an obvious spike", {
  expect_true(hampel_spike[21])
})

test_that("hampel_anomalies detects spikes at both ends of the interior", {
  expect_true(hampel_two_spikes[11]) # positive spike
  expect_true(hampel_two_spikes[30]) # negative spike
})

test_that("hampel_anomalies does not flag endpoints (within bandwidth of ends)", {
  bw <- 3
  expect_false(any(hampel_spike[1:bw]))
  expect_false(any(hampel_spike[(length(y_spike) - bw + 1):length(y_spike)]))
})

test_that("hampel_anomalies flags fewer points with larger k", {
  result_low_k <- hampel_anomalies(y_spike, bandwidth = 3, k = 1)
  result_high_k <- hampel_anomalies(y_spike, bandwidth = 3, k = 10)
  expect_gte(sum(result_low_k), sum(result_high_k))
})

test_that("hampel_anomalies flags fewer points with larger bandwidth", {
  # Wider window → larger local MAD → harder to be an outlier
  result_narrow <- hampel_anomalies(y_spike, bandwidth = 2, k = 3)
  result_wide <- hampel_anomalies(y_spike, bandwidth = 10, k = 3)
  expect_gte(sum(result_narrow), sum(result_wide))
})

test_that("hampel_anomalies errors on non-integer bandwidth", {
  expect_error(hampel_anomalies(y_clean, bandwidth = 2.5))
})

test_that("hampel_anomalies accepts integer-valued numeric bandwidth", {
  expect_no_error(hampel_anomalies(y_clean, bandwidth = 3))
  expect_no_error(hampel_anomalies(y_clean, bandwidth = 3L))
})

test_that("hampel_anomalies produces no false positives on clean normal data", {
  # With k=3 and clean data almost no points should be flagged
  expect_lt(sum(hampel_clean_k3), 5)
})

test_that("hampel_anomalies works with dplyr mutate and filter", {
  expect_s3_class(df_spike, "tbl_df")
  expect_true(df_spike$is_anomaly[21])
  anomalies <- df_spike |> dplyr::filter(is_anomaly)
  expect_true(20 %in% anomalies$y)
})
