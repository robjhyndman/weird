test_that("multiplication works", {
  y <- c(rnorm(10),10)
  expect_equal(
    max(kde_scores(y, kernel="gaussian", h=1)),
    log(11/weird:::K0("gaussian", h=1)),
    tolerance = 1e-5
  )
  expect_equal(
    max(kde_scores(y, kernel="epanechnikov", h=1)),
    log(11/weird:::K0("epanechnikov", h=1)),
    tolerance = 1e-5
  )
})
