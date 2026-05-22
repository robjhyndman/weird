test_that("dist_mclust", {
  # Univariate tests
  library(mclust)
  d1 <- Mclust(rnorm(100)) |> dist_mclust()
  expect_equal(family(d1), "normal")
  # Equal variance mixture
  d2 <- Mclust(c(rnorm(100), rnorm(100, 5, 1))) |> dist_mclust()
  expect_equal(family(d2), "mixture")
  # Unequal variance mixture
  d3 <- Mclust(c(rnorm(100), rnorm(100, 5, 2))) |> dist_mclust()
  expect_equal(family(d3), "mixture")
  # Multivariate tests
  d1 <- Mclust(n01[, 4:6]) |> dist_mclust()
  expect_equal(family(d1), "mvnorm")
  n01[500:1001, 1] <- n01[500:1001, 1] + 3
  d2 <- Mclust(n01[, 1:3]) |> dist_mclust()
  expect_equal(family(d2), "mixture")
  expect_equal(parameters(d2)$w[[1]], c(0.5, 0.5), tolerance = 0.05)
})
