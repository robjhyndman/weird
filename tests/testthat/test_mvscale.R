# Tests for mvscale()

# Shared fixtures -------------------------------------------------------
set.seed(7)
v_num <- rnorm(50)
v_const <- rep(3, 50) # constant column (scale = 0)
mat2 <- matrix(rnorm(100), ncol = 2)
mat3 <- matrix(rnorm(150), ncol = 3)
df_num <- as.data.frame(mat2)
df_mix <- data.frame(x = rnorm(50), y = rnorm(50), label = letters[1:50]) # two numeric + one character

# Vector input ----------------------------------------------------------

test_that("mvscale returns a numeric vector for vector input", {
  z <- mvscale(v_num, warning = FALSE)
  expect_type(z, "double")
  expect_length(z, length(v_num))
})

test_that("mvscale centres vector to approximately zero median", {
  z <- mvscale(v_num, warning = FALSE)
  expect_equal(median(z), 0, tolerance = 1e-6)
})

test_that("mvscale terms agrees with results from default arguments", {
  z <- mvscale(v_num, warning = FALSE, what = "terms")
  expect_equal(median(v_num), z$center, tolerance = 1e-6)
  expect_equal(robustbase::s_Qn(v_num), z$scale, tolerance = 1e-6)
})

test_that("mvscale errors on non-numeric vector", {
  expect_error(mvscale(letters), "numeric")
})

test_that("mvscale handles constant vector without error (scale = 0 guard)", {
  expect_no_error(mvscale(v_const, warning = FALSE))
})

# Matrix input ----------------------------------------------------------

test_that("mvscale returns a matrix of the same dimensions", {
  z <- mvscale(mat2, warning = FALSE)
  expect_true(is.matrix(z))
  expect_equal(dim(z), dim(mat2))
})

test_that("mvscale errors on non-numeric matrix", {
  m <- matrix(as.character(mat2), nrow = nrow(mat2))
  expect_error(mvscale(m), "numeric")
})

# Data frame input ------------------------------------------------------

test_that("mvscale returns a data frame for data frame input", {
  z <- mvscale(df_num, warning = FALSE)
  expect_s3_class(z, "data.frame")
  expect_equal(dim(z), dim(df_num))
})

test_that("mvscale renames numeric columns z1, z2, ... when rotation applied (data frame)", {
  z <- mvscale(df_num, warning = FALSE)
  expect_equal(names(z), paste0("z", seq_len(ncol(df_num))))
})

test_that("mvscale warns about non-numeric columns in a data frame", {
  expect_warning(mvscale(df_mix), "non-numeric")
})

test_that("mvscale warning = FALSE suppresses non-numeric column warning", {
  expect_no_warning(mvscale(df_mix, warning = FALSE))
})

test_that("mvscale retains non-numeric columns in the output", {
  z <- suppressWarnings(mvscale(df_mix))
  expect_true("label" %in% names(z))
  expect_equal(z$label, df_mix$label)
})

# center = NULL ---------------------------------------------------------

test_that("mvscale with center = NULL returns correct dimensions", {
  z <- mvscale(mat2, center = NULL, warning = FALSE)
  expect_true(is.matrix(z))
  expect_equal(dim(z), dim(mat2))
})

# cov = NULL (per-column scaling, no rotation) --------------------------

test_that("mvscale with cov = NULL returns same-dimension matrix, no rotation", {
  z <- mvscale(mat2, cov = NULL, warning = FALSE)
  expect_true(is.matrix(z))
  expect_equal(dim(z), dim(mat2))
})

test_that("mvscale with cov = NULL does not rename columns (data frame)", {
  z <- mvscale(df_num, cov = NULL, warning = FALSE)
  expect_equal(names(z), names(df_num))
})

test_that("mvscale with cov = NULL scales each column by s_Qn", {
  z <- mvscale(mat2, cov = NULL, warning = FALSE)
  # After centering and scaling by s_Qn, s_Qn of each column should be ~1
  col_scales <- apply(z, 2, robustbase::s_Qn)
  expect_equal(col_scales, c(1, 1), tolerance = 0.01)
})

test_that("mvscale with cov = NULL scales each column by s_Qn", {
  z <- mvscale(mat2, cov = NULL, warning = FALSE, what = "terms")
  col_scales <- apply(mat2, 2, robustbase::s_Qn)
  expect_equal(z$scale, col_scales, tolerance = 0.01)
})

test_that("mvscale with cov = NULL centers each column by median", {
  z <- mvscale(mat2, cov = NULL, warning = FALSE, what = "terms")
  col_centers <- apply(mat2, 2, median)
  expect_equal(z$center, col_centers, tolerance = 0.01)
})

# cov = stats::cov (non-robust rotation) --------------------------------

test_that("mvscale with cov = stats::cov returns correct dimensions", {
  z <- mvscale(mat3, cov = stats::cov, warning = FALSE)
  expect_true(is.matrix(z))
  expect_equal(dim(z), dim(mat3))
})

# center / scale overrides ----------------------------------------------

test_that("mvscale with center = mean, scale = sd, cov = NULL gives standard z-scores", {
  z <- mvscale(v_num, center = mean, scale = sd, cov = NULL, warning = FALSE)
  expect_equal(mean(z), 0, tolerance = 1e-6)
  expect_equal(sd(z), 1, tolerance = 1e-6)
})

# Constant column guard -------------------------------------------------

test_that("mvscale with a constant column in a matrix does not error", {
  mat_const <- cbind(rnorm(50), rep(5, 50))
  expect_no_error(mvscale(mat_const, cov = NULL, warning = FALSE))
})

# Missing values --------------------------------------------------------

test_that("mvscale propagates NA to all columns in the same row (default cov)", {
  mat_na <- mat2
  mat_na[3, 1] <- NA
  z <- mvscale(mat_na, warning = FALSE)
  expect_true(is.na(z[3, 1]))
  expect_true(is.na(z[3, 2]))
  expect_false(anyNA(z[-3, ]))
})

test_that("mvscale preserves NA positions in vector input", {
  v_na <- v_num
  v_na[c(2, 10)] <- NA
  z <- mvscale(v_na, warning = FALSE)
  expect_true(all(is.na(z[c(2, 10)])))
  expect_false(anyNA(z[-c(2, 10)]))
})

test_that("mvscale propagates NA to all numeric columns in the same row (data frame)", {
  df_na <- df_mix
  df_na[5, "x"] <- NA
  z <- suppressWarnings(mvscale(df_na))
  expect_true(is.na(z[5, "z1"]))
  expect_true(is.na(z[5, "z2"]))
  expect_equal(z[5, "label"], df_na[5, "label"])
})

# Infinite values -------------------------------------------------------

test_that("mvscale errors on Inf in a vector", {
  v_inf <- v_num
  v_inf[5] <- Inf
  expect_snapshot(mvscale(v_inf, warning = FALSE), error = TRUE)
})

test_that("mvscale errors on Inf in a matrix", {
  mat_inf <- mat2
  mat_inf[1, 2] <- Inf
  expect_snapshot(mvscale(mat_inf, warning = FALSE), error = TRUE)
})

test_that("mvscale errors on Inf in a data frame", {
  df_inf <- df_num
  df_inf[1, 1] <- Inf
  expect_snapshot(mvscale(df_inf, warning = FALSE), error = TRUE)
})
