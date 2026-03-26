# Tests for hdr.R: hdr_table(), hdr_palette(), gg_hdrboxplot()

# hdr_palette() ----------------------------------------------------------

test_that("hdr_palette returns correct number of colors", {
  pal <- hdr_palette(prob = c(0.5, 0.99))
  expect_length(pal, 3L) # one per prob level + mode color
})

test_that("hdr_palette first color equals supplied color", {
  col <- "#FF0000"
  pal <- hdr_palette(color = col, prob = c(0.5, 0.99))
  expect_equal(pal[1], col)
})

test_that("hdr_palette errors on invalid prob", {
  expect_error(hdr_palette(prob = c(-0.1, 0.5)), "prob must be between 0 and 1")
  expect_error(hdr_palette(prob = c(0.5, 1.1)), "prob must be between 0 and 1")
})

test_that("hdr_palette works with n and no prob", {
  pal <- hdr_palette(n = 4, color = "#0072b2")
  expect_length(pal, 4L)
})

# hdr_table() univariate --------------------------------------------------

set.seed(1)
result_1d <- hdr_table(dist_kde(rnorm(200)), prob = c(0.5, 0.95))

test_that("hdr_table returns tibble with correct columns (1d)", {
  expect_s3_class(result_1d, "tbl_df")
  expect_true(all(
    c("prob", "density", "lower", "upper") %in% names(result_1d)
  ))
})

test_that("hdr_table density values are positive (1d)", {
  expect_true(all(result_1d$density > 0))
})

test_that("hdr_table lower < upper (1d)", {
  expect_true(all(result_1d$lower < result_1d$upper))
})

test_that("hdr_table higher prob gives lower density threshold (1d)", {
  den_50 <- result_1d$density[result_1d$prob == 0.50][1]
  den_95 <- result_1d$density[result_1d$prob == 0.95][1]
  expect_gt(den_50, den_95)
})

test_that("hdr_table works with multiple distributions (1d)", {
  set.seed(1)
  dist <- dist_kde(list(rnorm(200), rnorm(200, 5)))
  result <- hdr_table(dist, prob = 0.9)
  expect_equal(length(unique(result$distribution)), 2L)
})

test_that("hdr_table works with dist_normal (1d)", {
  dist <- distributional::dist_normal()
  result <- hdr_table(dist, prob = 0.95)
  expect_s3_class(result, "tbl_df")
  expect_true(all(result$lower < result$upper))
})

result_normal <- hdr_table(
  distributional::dist_normal(mu = 0, sigma = 1),
  prob = c(0.50, 0.95)
)
res50 <- result_normal[result_normal$prob == 0.50, ]
res95 <- result_normal[result_normal$prob == 0.95, ]

test_that("hdr_table lower/upper/density match analytic values for dist_normal", {
  expect_equal(res50$lower, qnorm(0.25), tolerance = 1e-3)
  expect_equal(res50$upper, qnorm(0.75), tolerance = 1e-3)
  expect_equal(res95$lower, qnorm(0.025), tolerance = 1e-3)
  expect_equal(res95$upper, qnorm(0.975), tolerance = 1e-3)
  expect_equal(res95$density, dnorm(qnorm(0.025)), tolerance = 1e-4)
})

result_shifted <- hdr_table(
  distributional::dist_normal(mu = 5, sigma = 2),
  prob = c(0.50, 0.95)
)
res50 <- result_shifted[result_shifted$prob == 0.50, ]
res95 <- result_shifted[result_shifted$prob == 0.95, ]

test_that("hdr_table analytic values correct for shifted dist_normal", {
  expect_equal(res50$lower, qnorm(0.25, mean = 5, sd = 2), tolerance = 1e-3)
  expect_equal(res50$upper, qnorm(0.75, mean = 5, sd = 2), tolerance = 1e-3)
  expect_equal(res95$lower, qnorm(0.025, mean = 5, sd = 2), tolerance = 1e-3)
  expect_equal(res95$upper, qnorm(0.975, mean = 5, sd = 2), tolerance = 1e-3)
})

# hdr_table() bivariate ---------------------------------------------------

result_2d <- hdr_table(dist_kde(cbind(rnorm(200), rnorm(200))), prob = 0.9)

test_that("hdr_table returns tibble with correct columns (2d)", {
  expect_s3_class(result_2d, "tbl_df")
  expect_true(all(c("prob", "density") %in% names(result_2d)))
  expect_false("lower" %in% names(result_2d))
})

test_that("hdr_table density positive (2d)", {
  expect_true(all(result_2d$density > 0))
})

# gg_hdrboxplot() ---------------------------------------------------------

df_1d <- data.frame(x = rnorm(100))

test_that("gg_hdrboxplot returns a ggplot for 1d data", {
  expect_s3_class(gg_hdrboxplot(df_1d, x), "ggplot")
})

test_that("gg_hdrboxplot works with show_points = TRUE", {
  expect_s3_class(gg_hdrboxplot(df_1d, x, show_points = TRUE), "ggplot")
})

test_that("gg_hdrboxplot works with show_anomalies = FALSE", {
  expect_s3_class(gg_hdrboxplot(df_1d, x, show_anomalies = FALSE), "ggplot")
})

test_that("gg_hdrboxplot works with custom prob", {
  expect_s3_class(
    gg_hdrboxplot(df_1d, x, prob = c(0.25, 0.75, 0.99)),
    "ggplot"
  )
})

test_that("gg_hdrboxplot selects first variable when var1 is missing", {
  expect_message(
    p <- gg_hdrboxplot(df_1d),
    regexp = NA # no message expected for single-column data frame
  )
  expect_s3_class(p, "ggplot")
})

df_2d <- data.frame(x = rnorm(100), y = rnorm(100))

test_that("gg_hdrboxplot returns a ggplot for 2d data", {
  expect_s3_class(gg_hdrboxplot(df_2d, x, y), "ggplot")
})

test_that("gg_hdrboxplot messages when no variable selected (>1 column)", {
  expect_message(gg_hdrboxplot(df_2d), "No variable selected")
})

test_that("gg_hdrboxplot works with a named dataset", {
  p <- oldfaithful |> gg_hdrboxplot(duration)
  expect_s3_class(p, "ggplot")
})

test_that("gg_hdrboxplot 2d works with oldfaithful", {
  p <- oldfaithful |> gg_hdrboxplot(duration, waiting)
  expect_s3_class(p, "ggplot")
})
