test_that("density_df", {
  set.seed(1)
  # Univariate normal
  df <- dist_normal() |> density_df()
  expect_equal(colnames(df), c("x", "distribution", "density"))
  expect_equal(dim(df), c(501L, 3L))
  # Univariate kde
  df <- dist_kde(rnorm(100)) |> density_df()
  expect_equal(colnames(df), c("x", "distribution", "density"))
  expect_equal(dim(df), c(501L, 3L))
  # Univariate discrete
  df <- dist_poisson(4) |> density_df()
  expect_equal(dim(df), c(12L, 3L))
  # Multiple distributions
  df <- c(dist_poisson(4), dist_normal(1)) |> density_df()
  expect_equal(unique(df$distribution), c("Pois(4)", "N(1, 1)"))
  # Multivariate normal with 1d
  df <- dist_multivariate_normal() |> density_df()
  expect_equal(dim(df), c(501L, 3L))
  # Bivariate distribution
  df <- dist_multivariate_normal(mu = list(c(0, 0)), sigma = list(diag(2))) |>
    density_df()
  expect_equal(dim(df), c(10201L, 4L))
  # Multiple bivariate distributions
  df <- c(
    dist_multivariate_normal(mu = list(c(0, 0)), sigma = list(diag(2))),
    dist_multivariate_normal()
  ) |>
    density_df()
  expect_equal(dim(df), c(501L + 10201L, 4L))
  df <- c(
    dist_multivariate_normal(mu = list(c(0, 0)), sigma = list(diag(2))),
    dist_multivariate_normal(mu = list(c(1,1)), sigma = list(diag(2)))
  ) |>
    density_df()
  expect_equal(dim(df), c(2 * 10201L, 4L))
  expect_equal(length(unique(df$distribution)), 2L)
  # Trivariate distribution
  dist <- dist_multivariate_normal(mu = list(rep(0, 3)), sigma = list(diag(3)))
  expect_error(density_df(dist))
})
