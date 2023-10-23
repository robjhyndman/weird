test_that("multiplication works", {
  y <- c(rnorm(10), 100)
  expect_equal(
    max(kde_scores(y, h = 1)),
    log(11 / dnorm(0, 0, 1)),
  )
})
