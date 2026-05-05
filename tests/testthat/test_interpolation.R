# Setup: x and y grids with different lengths to exercise both dimensions
x <- 1:3
y <- 1:4
z_linear <- outer(x, y, "+") # z[i,j] = x[i] + y[j]   — linear, exact
z_bilinear <- outer(x, y) # z[i,j] = x[i] * y[j]   — bilinear, exact
z_const <- matrix(5, nrow = 3, ncol = 4)

# --- Correctness --------------------------------------------------------------

test_that("bilinear_interpolation is exact at interior grid nodes", {
  expect_equal(weird:::bilinear_interpolation(x, y, z_linear, 1, 1), 2) # 1+1
  expect_equal(weird:::bilinear_interpolation(x, y, z_linear, 2, 3), 5) # 2+3
})

test_that("bilinear_interpolation is exact for linear surfaces", {
  expect_equal(weird:::bilinear_interpolation(x, y, z_linear, 1.5, 2.5), 4.0)
  expect_equal(weird:::bilinear_interpolation(x, y, z_linear, 2.3, 3.7), 6.0)
})

test_that("bilinear_interpolation is exact for bilinear surfaces", {
  # z = x * y is bilinear, so the formula reproduces it exactly
  expect_equal(weird:::bilinear_interpolation(x, y, z_bilinear, 1.5, 2.5), 3.75)
  expect_equal(weird:::bilinear_interpolation(x, y, z_bilinear, 2.5, 2.5), 6.25)
})

test_that("bilinear_interpolation returns constant on a flat surface", {
  expect_equal(weird:::bilinear_interpolation(x, y, z_const, 1.5, 2.5), 5)
  expect_equal(weird:::bilinear_interpolation(x, y, z_const, 2.8, 3.1), 5)
})

# --- Vectorisation ------------------------------------------------------------

test_that("bilinear_interpolation vectorises over (x0, y0)", {
  result <- weird:::bilinear_interpolation(
    x,
    y,
    z_bilinear,
    c(1.5, 2.5),
    c(1.5, 2.5)
  )
  expect_equal(result, c(2.25, 6.25))
})

test_that("bilinear_interpolation returns a numeric vector of length n", {
  x0 <- c(1.2, 1.8, 2.4)
  y0 <- c(1.5, 2.5, 3.5)
  result <- weird:::bilinear_interpolation(x, y, z_linear, x0, y0)
  expect_type(result, "double")
  expect_length(result, 3L)
})

# --- Out-of-bounds behaviour --------------------------------------------------

test_that("bilinear_interpolation returns NA for points outside the grid", {
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 0, 2))) # x too low
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 4, 2))) # x too high
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 1.5, 0))) # y too low
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 1.5, 5))) # y too high
})

test_that("bilinear_interpolation returns NA for the upper boundary nodes", {
  # The algorithm indexes into cell [i, i+1], so the last grid line is out of range
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 3, 2))) # x = x[nx]
  expect_true(is.na(weird:::bilinear_interpolation(x, y, z_linear, 2, 4))) # y = y[ny]
})

test_that("bilinear_interpolation handles mixed in/out-of-bounds vectors", {
  result <- weird:::bilinear_interpolation(
    x,
    y,
    z_linear,
    c(1.5, 0, 2.5),
    c(2, 2, 2)
  )
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

# --- Error handling -----------------------------------------------------------

test_that("bilinear_interpolation errors when z dimensions mismatch x and y", {
  bad_z <- matrix(1:6, nrow = 2, ncol = 3) # 2×3 but x has length 3
  expect_error(weird:::bilinear_interpolation(x, y, bad_z, 1.5, 2.5))
})

test_that("bilinear_interpolation errors when x0 and y0 have different lengths", {
  expect_error(weird:::bilinear_interpolation(x, y, z_linear, c(1.5, 2.0), 2.5))
})
