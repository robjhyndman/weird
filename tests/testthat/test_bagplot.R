# Tests for bagplot.R: gg_bagplot()

set.seed(42)
df_bivar <- data.frame(v1 = rnorm(100), v2 = rnorm(100))

# Return type ---------------------------------------------------------------

test_that("gg_bagplot returns a ggplot object", {
  p <- gg_bagplot(df_bivar, v1, v2)
  expect_s3_class(p, "ggplot")
})

# Works with package datasets -----------------------------------------------

test_that("gg_bagplot works with n01 dataset", {
  p <- gg_bagplot(n01, v1, v2)
  expect_s3_class(p, "ggplot")
})

test_that("gg_bagplot with show_points works with n01 dataset", {
  p <- gg_bagplot(n01, v1, v2, show_points = TRUE)
  expect_s3_class(p, "ggplot")
})

# Polygon layers (default bagplot) ------------------------------------------

test_that("gg_bagplot default contains geom_polygon layers", {
  p <- gg_bagplot(df_bivar, v1, v2)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(layer_classes == "GeomPolygon"))
})

test_that("gg_bagplot default contains a geom_point layer for median", {
  p <- gg_bagplot(df_bivar, v1, v2)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(layer_classes == "GeomPoint"))
})

# Point layers (show_points = TRUE) -----------------------------------------

test_that("gg_bagplot show_points = TRUE contains geom_point layers", {
  p <- gg_bagplot(df_bivar, v1, v2, show_points = TRUE)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(layer_classes == "GeomPoint"))
})

test_that("gg_bagplot show_points = TRUE contains no geom_polygon layers", {
  p <- gg_bagplot(df_bivar, v1, v2, show_points = TRUE)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(layer_classes == "GeomPolygon"))
})

# Axis labels reflect variable names ----------------------------------------

test_that("gg_bagplot axis mappings reflect the supplied variable names", {
  p <- gg_bagplot(df_bivar, v1, v2)
  expect_equal(rlang::as_label(p$mapping$x), "v1")
  expect_equal(rlang::as_label(p$mapping$y), "v2")
})

# Custom color ---------------------------------------------------------------

test_that("gg_bagplot accepts a custom color without error", {
  expect_no_error(gg_bagplot(df_bivar, v1, v2, color = "#FF0000"))
})


# Matrix input --------------------------------------------------------------

test_that("gg_bagplot works when data is a matrix coerced to data frame", {
  mat <- as.data.frame(matrix(
    rnorm(200),
    ncol = 2,
    dimnames = list(NULL, c("v1", "v2"))
  ))
  expect_s3_class(gg_bagplot(mat, v1, v2), "ggplot")
})
