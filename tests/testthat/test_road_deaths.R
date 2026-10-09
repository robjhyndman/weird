test_that("road_deaths has expected dimensions and column names", {
  expect_equal(nrow(road_deaths), 58168L)
  expect_named(
    road_deaths,
    c(
      "crash_id",
      "state",
      "year",
      "month",
      "day_of_week",
      "time",
      "crash_type",
      "bus",
      "heavy_rigid_truck",
      "articulated_truck",
      "speed_limit",
      "road_user",
      "gender",
      "age",
      "remoteness",
      "sa4",
      "lga",
      "road_type",
      "christmas",
      "easter"
    )
  )
})

test_that("road_deaths has expected column types", {
  expect_type(road_deaths$crash_id, "character")
  expect_s3_class(road_deaths$state, "factor")
  expect_s3_class(road_deaths$day_of_week, "factor")
  expect_type(road_deaths$time, "double")
  expect_type(road_deaths$bus, "logical")
  expect_type(road_deaths$speed_limit, "integer")
  expect_type(road_deaths$age, "integer")
})

test_that("road_deaths values are in valid ranges", {
  expect_all_true(road_deaths$year %in% 1989:2025)
  expect_all_true(road_deaths$month %in% 1:12)
  expect_true(all(road_deaths$age >= 0 & road_deaths$age <= 110, na.rm = TRUE))
  expect_true(all(road_deaths$speed_limit > 0, na.rm = TRUE))
  expect_true(all(road_deaths$time >= 0 & road_deaths$time < 24, na.rm = TRUE))
  expect_false(anyNA(road_deaths$state))
})
