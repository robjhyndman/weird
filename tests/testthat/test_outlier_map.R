# Tests for outlier_map.R: outlier_map()

set.seed(42)
Y <- data.frame(v1 = rnorm(100), v2 = rnorm(100), v3 = rnorm(100))
pca <- prcomp(Y, scale = TRUE, rank. = 2)

# Return type ---------------------------------------------------------------

test_that("outlier_map returns a ggplot object for a prcomp object", {
  expect_s3_class(outlier_map(pca, data = Y), "ggplot")
})

test_that("outlier_map returns a ggplot object for scores and loadings", {
  expect_s3_class(
    outlier_map(scores = pca$x, loadings = pca$rotation, data = Y),
    "ggplot"
  )
})

# Distances -----------------------------------------------------------------

test_that("outlier_map computes score and orthogonal distances for prcomp", {
  p <- outlier_map(pca, data = Y)
  sc <- pca$x
  sd_manual <- sqrt(rowSums(sweep(sc^2, 2, pca$sdev[1:2]^2, "/")))
  z <- scale(as.matrix(Y), pca$center, pca$scale)
  od_manual <- sqrt(rowSums((z - sc %*% t(pca$rotation))^2))
  expect_equal(unname(p$data$sd), unname(sd_manual))
  expect_equal(unname(p$data$od), unname(od_manual))
})

# Thresholds ----------------------------------------------------------------

test_that("outlier_map draws cutoff lines and colours by type when shown", {
  p <- outlier_map(pca, data = Y)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_all_true(c("GeomPoint", "GeomVline", "GeomHline") %in% geoms)
  expect_setequal(
    levels(p$data$type),
    c(
      "Orthogonal outlier",
      "Regular",
      "Bad leverage point",
      "Good leverage point"
    )
  )
})

test_that("show_thresholds = FALSE drops cutoff lines and type colours", {
  p <- outlier_map(pca, data = Y, show_thresholds = FALSE)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_setequal(geoms, "GeomPoint")
  expect_setequal(names(p$data), c("sd", "od"))
})

test_that("thresholds are ignored when scores and loadings are passed", {
  p <- outlier_map(scores = pca$x, loadings = pca$rotation, data = Y)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_setequal(geoms, "GeomPoint")
  expect_setequal(names(p$data), c("sd", "od"))
})

# Classification ------------------------------------------------------------

test_that("outlier_map classifies observations by the cutoffs", {
  p <- outlier_map(pca, data = Y)
  expected <- ifelse(
    p$data$sd <= sqrt(qchisq(0.975, 2)) & p$data$od <= od_cutoff(p$data$od),
    "Regular",
    ifelse(
      p$data$sd > sqrt(qchisq(0.975, 2)) & p$data$od <= od_cutoff(p$data$od),
      "Good leverage point",
      ifelse(
        p$data$od > od_cutoff(p$data$od) &
          p$data$sd <= sqrt(qchisq(0.975, 2)),
        "Orthogonal outlier",
        "Bad leverage point"
      )
    )
  )
  expect_equal(as.character(p$data$type), expected)
})

# Axis labels ---------------------------------------------------------------

test_that("outlier_map labels the axes", {
  p <- outlier_map(pca, data = Y)
  expect_equal(p$labels$x, "Score distance")
  expect_equal(p$labels$y, "Orthogonal distance")
})

# rrcov objects -------------------------------------------------------------

test_that("outlier_map extracts distances from an rrcov Pca object", {
  skip_if_not_installed("rrcov")
  pc <- rrcov::PcaClassic(as.matrix(Y), k = 2, scale = TRUE)
  p <- outlier_map(pc)
  expect_s3_class(p, "ggplot")
  expect_equal(unname(p$data$sd), unname(pc$sd))
  expect_equal(unname(p$data$od), unname(pc$od))
})

# Errors --------------------------------------------------------------------

test_that("outlier_map errors for an unsupported object", {
  expect_snapshot(
    error = TRUE,
    outlier_map(object = lm(v1 ~ v2, Y))
  )
})

test_that("outlier_map errors when data is missing for a prcomp object", {
  expect_snapshot(error = TRUE, outlier_map(pca))
})

test_that("outlier_map errors when scores or loadings are missing", {
  expect_snapshot(error = TRUE, outlier_map(scores = pca$x, data = Y))
})
