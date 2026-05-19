test_that("air_quality has expected column names", {
  skip_if(is.null(air_quality_raw))
  expect_named(
    air_quality_raw,
    c(
      "station",
      "year",
      "month",
      "day",
      "hour",
      "pm2_5",
      "pm10",
      "so2",
      "no2",
      "co",
      "o3",
      "temperature",
      "pressure",
      "dew_point",
      "rainfall",
      "wind_direction",
      "wind_speed"
    )
  )
})

test_that("air_quality has expected dimensions", {
  skip_if(is.null(air_quality_raw))
  expect_equal(nrow(air_quality_raw), 420768L)
  expect_equal(ncol(air_quality_raw), 17L)
})

test_that("air_quality has expected column types", {
  skip_if(is.null(air_quality_raw))
  expect_type(air_quality_raw$station, "character")
  expect_type(air_quality_raw$year, "double")
  expect_type(air_quality_raw$wind_direction, "character")
  expect_type(air_quality_raw$pm2_5, "double")
  expect_type(air_quality_raw$temperature, "double")
})

test_that("air_quality has expected stations", {
  skip_if(is.null(air_quality_raw))
  expected_stations <- c(
    "Aotizhongxin",
    "Changping",
    "Dingling",
    "Dongsi",
    "Guanyuan",
    "Gucheng",
    "Huairou",
    "Nongzhanguan",
    "Shunyi",
    "Tiantan",
    "Wanliu",
    "Wanshouxigong"
  )
  expect_setequal(unique(air_quality_raw$station), expected_stations)
})

test_that("air_quality time variables are in valid ranges", {
  skip_if(is.null(air_quality_raw))
  expect_all_true(air_quality_raw$year %in% 2013:2017)
  expect_all_true(air_quality_raw$month %in% 1:12)
  expect_all_true(air_quality_raw$day %in% 1:31)
  expect_all_true(air_quality_raw$hour %in% 0:23)
})

test_that("air_quality pollutant concentrations are non-negative", {
  skip_if(is.null(air_quality_raw))
  non_neg <- function(x) all(x >= 0, na.rm = TRUE)
  expect_true(non_neg(air_quality_raw$pm2_5))
  expect_true(non_neg(air_quality_raw$pm10))
  expect_true(non_neg(air_quality_raw$so2))
  expect_true(non_neg(air_quality_raw$no2))
  expect_true(non_neg(air_quality_raw$co))
  expect_true(non_neg(air_quality_raw$o3))
  expect_true(non_neg(air_quality_raw$rainfall))
  expect_true(non_neg(air_quality_raw$wind_speed))
})
