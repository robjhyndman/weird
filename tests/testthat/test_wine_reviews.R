test_that("wine_reviews has expected column names", {
  skip_if(is.null(wine_reviews_raw))
  expect_named(
    wine_reviews_raw,
    c(
      "country",
      "state",
      "region",
      "winery",
      "variety",
      "year",
      "points",
      "price"
    )
  )
})

test_that("wine_reviews has expected dimensions", {
  skip_if(is.null(wine_reviews_raw))
  expect_equal(nrow(wine_reviews_raw), 110203L)
  expect_equal(ncol(wine_reviews_raw), 8L)
})

test_that("wine_reviews has expected column types", {
  skip_if(is.null(wine_reviews_raw))
  expect_type(wine_reviews_raw$country, "character")
  expect_type(wine_reviews_raw$state, "character")
  expect_type(wine_reviews_raw$region, "character")
  expect_type(wine_reviews_raw$winery, "character")
  expect_type(wine_reviews_raw$variety, "character")
  expect_type(wine_reviews_raw$points, "double")
  expect_type(wine_reviews_raw$price, "double")
  expect_type(wine_reviews_raw$year, "double")
})

test_that("wine_reviews points are in range 80-100", {
  skip_if(is.null(wine_reviews_raw))
  expect_true(all(wine_reviews_raw$points >= 80, na.rm = TRUE))
  expect_true(all(wine_reviews_raw$points <= 100, na.rm = TRUE))
})

test_that("wine_reviews prices are positive", {
  skip_if(is.null(wine_reviews_raw))
  expect_true(all(wine_reviews_raw$price > 0, na.rm = TRUE))
})

test_that("wine_reviews years are plausible", {
  skip_if(is.null(wine_reviews_raw))
  expect_true(all(wine_reviews_raw$year >= 1900, na.rm = TRUE))
  expect_true(all(wine_reviews_raw$year <= 2017, na.rm = TRUE))
})
