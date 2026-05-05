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

# Outlier rendering -----------------------------------------------------------

test_that("gg_bagplot renders clear outliers as a black geom_point layer", {
  set.seed(1)
  df_out <- data.frame(
    v1 = c(rnorm(100), 15, -15),
    v2 = c(rnorm(100), 15, -15)
  )
  p <- gg_bagplot(df_out, v1, v2)
  # Fixed colour is stored as aes_params$colour after ggplot2 alias normalisation
  point_colours <- vapply(
    Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers),
    function(l) l$aes_params$colour,
    character(1)
  )
  expect_true("#000000" %in% point_colours)
})

# NA values -------------------------------------------------------------------

test_that("gg_bagplot handles NA values without error", {
  set.seed(1)
  df_na <- data.frame(v1 = c(rnorm(50), NA), v2 = c(rnorm(50), NA))
  expect_no_error(gg_bagplot(df_na, v1, v2))
  expect_s3_class(gg_bagplot(df_na, v1, v2), "ggplot")
})

# Very small n ----------------------------------------------------------------

test_that("gg_bagplot errors gracefully with fewer than 4 observations", {
  df_tiny <- data.frame(v1 = 1:3, v2 = 1:3)
  expect_error(gg_bagplot(df_tiny, v1, v2))
})
