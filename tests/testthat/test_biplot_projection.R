# Tests for biplot_projection.R: biplot_projection()

set.seed(42)
df_bivar <- data.frame(v1 = rnorm(100), v2 = rnorm(100), v3 = rnorm(100))
pca <- prcomp(df_bivar, scale = TRUE)

# Return type ---------------------------------------------------------------

test_that("biplot_projection returns a ggplot object for a prcomp object", {
  expect_s3_class(biplot_projection(pca), "ggplot")
})

test_that("biplot_projection returns a ggplot object for scores and loadings", {
  expect_s3_class(
    biplot_projection(scores = pca$x, loadings = pca$rotation),
    "ggplot"
  )
})

# Layers --------------------------------------------------------------------

test_that("biplot_projection contains point, segment and text layers", {
  p <- biplot_projection(pca)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_all_true(
    c("GeomPoint", "GeomSegment", "GeomText") %in% layer_classes
  )
})

# Axis labels ---------------------------------------------------------------

test_that("biplot_projection labels axes with the first two score columns", {
  p <- biplot_projection(pca)
  expect_equal(p$labels$x, "PC1")
  expect_equal(p$labels$y, "PC2")
})

# label_threshold -----------------------------------------------------------

test_that("label_threshold drops short loadings from the text layer", {
  loadings <- matrix(
    c(1, 0, 0.01, 0, 1, 0.01),
    ncol = 2,
    dimnames = list(c("a", "b", "c"), NULL)
  )
  scores <- matrix(rnorm(40), ncol = 2)
  p <- biplot_projection(
    scores = scores,
    loadings = loadings,
    label_threshold = 0.5
  )
  text_layer <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
  expect_setequal(text_layer$data$varname, c("a", "b"))
})

# Custom colours ------------------------------------------------------------

test_that("biplot_projection accepts custom colours without error", {
  expect_no_error(
    biplot_projection(pca, point_colour = "#FF0000", arrow_colour = "#00FF00")
  )
})

# Invalid object ------------------------------------------------------------

test_that("biplot_projection errors for an unsupported object", {
  expect_snapshot(
    error = TRUE,
    biplot_projection(object = lm(v1 ~ v2, df_bivar))
  )
})
