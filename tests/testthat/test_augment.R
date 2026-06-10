# Tests for augment.R: augment.Pca()

set.seed(42)
Y <- data.frame(v1 = rnorm(100), v2 = rnorm(100), v3 = rnorm(100))

test_that("augment.Pca returns scores, score and orthogonal distances", {
  skip_if_not_installed("rrcov")
  pc <- rrcov::PcaClassic(as.matrix(Y), k = 2, scale = TRUE)
  aug <- broom::augment(pc)
  expect_s3_class(aug, "tbl_df")
  expect_named(aug, c(".fittedPC1", ".fittedPC2", ".sd", ".od"))
  expect_equal(aug$.sd, unname(pc$sd))
  expect_equal(aug$.od, unname(pc$od))
  expect_equal(aug$.fittedPC1, unname(pc$scores[, 1]))
})

test_that("augment.Pca binds the original data when supplied", {
  skip_if_not_installed("rrcov")
  pc <- rrcov::PcaClassic(as.matrix(Y), k = 2, scale = TRUE)
  aug <- broom::augment(pc, data = Y)
  expect_named(
    aug,
    c("v1", "v2", "v3", ".fittedPC1", ".fittedPC2", ".sd", ".od")
  )
  expect_equal(aug[c("v1", "v2", "v3")], dplyr::as_tibble(Y))
})

test_that("outlier_map uses augment distances for an rrcov Pca object", {
  skip_if_not_installed("rrcov")
  pc <- rrcov::PcaClassic(as.matrix(Y), k = 2, scale = TRUE)
  p <- outlier_map(pc)
  aug <- broom::augment(pc)
  expect_equal(p$data$sd, aug$.sd)
  expect_equal(p$data$od, aug$.od)
})
