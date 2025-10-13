test_that("multiplication works", {
  set.seed(2)
  y <- c(rnorm(10), 100)
  expect_identical(max(surprisals(y, dist_normal())), Inf)
  expect_identical(max(surprisals(y, h = 1, loo = TRUE)), Inf)
  expect_equal(
    max(surprisals(y, h = 1, loo = FALSE)),
    log(11 / dnorm(0, 0, 1)),
    tolerance = 0.01
  )
})
