# Pre-computed properties reused across tests
cricket_cols <- c(
  "Player",
  "Country",
  "Start",
  "End",
  "Matches",
  "Innings",
  "NotOuts",
  "Runs",
  "HighScore",
  "HighScoreNotOut",
  "Average",
  "Hundreds",
  "Fifties",
  "Ducks",
  "Gender"
)
oldfaithful_cols <- c("time", "recorded_duration", "duration", "waiting")
n01_cols <- paste0("v", 1:10)
fr_mortality_cols <- c("Year", "Age", "Sex", "Mortality")

# --- cricket_batting ----------------------------------------------------------

test_that("cricket_batting has expected column names", {
  expect_named(cricket_batting, cricket_cols)
})

test_that("cricket_batting has expected column types", {
  expect_type(cricket_batting$Player, "character")
  expect_type(cricket_batting$Country, "character")
  expect_type(cricket_batting$Start, "integer")
  expect_type(cricket_batting$End, "integer")
  expect_type(cricket_batting$Matches, "integer")
  expect_type(cricket_batting$Innings, "integer")
  expect_type(cricket_batting$NotOuts, "integer")
  expect_type(cricket_batting$Runs, "integer")
  expect_type(cricket_batting$HighScore, "integer")
  expect_type(cricket_batting$HighScoreNotOut, "logical")
  expect_type(cricket_batting$Average, "double")
  expect_type(cricket_batting$Hundreds, "integer")
  expect_type(cricket_batting$Fifties, "integer")
  expect_type(cricket_batting$Ducks, "integer")
  expect_type(cricket_batting$Gender, "character")
})

test_that("cricket_batting Gender values are Men or Women", {
  expect_setequal(unique(cricket_batting$Gender), c("Men", "Women"))
})

# --- oldfaithful --------------------------------------------------------------

test_that("oldfaithful has expected column names", {
  expect_named(oldfaithful, oldfaithful_cols)
})

test_that("oldfaithful has expected column types", {
  expect_s3_class(oldfaithful$time, "POSIXct")
  expect_type(oldfaithful$recorded_duration, "character")
  expect_type(oldfaithful$duration, "double")
  expect_type(oldfaithful$waiting, "double")
})

test_that("oldfaithful durations are positive", {
  expect_true(all(oldfaithful$duration > 0, na.rm = TRUE))
  expect_true(all(oldfaithful$waiting > 0, na.rm = TRUE))
})

# --- n01 ----------------------------------------------------------------------

test_that("n01 has expected column names", {
  expect_named(n01, n01_cols)
})

test_that("n01 columns are numeric", {
  expect_true(all(sapply(n01, is.numeric)))
})

# --- fr_mortality -------------------------------------------------------------

test_that("fr_mortality has expected column names", {
  expect_named(fr_mortality, fr_mortality_cols)
})

test_that("fr_mortality has expected column types", {
  expect_type(fr_mortality$Year, "integer")
  expect_type(fr_mortality$Age, "integer")
  expect_type(fr_mortality$Sex, "character")
  expect_type(fr_mortality$Mortality, "double")
})

test_that("fr_mortality years are in range 1816-1999", {
  expect_true(all(fr_mortality$Year >= 1816L))
  expect_true(all(fr_mortality$Year <= 1999L))
})

test_that("fr_mortality mortality rates are between 0 and 1", {
  expect_true(all(fr_mortality$Mortality >= 0, na.rm = TRUE))
  expect_true(all(fr_mortality$Mortality <= 1, na.rm = TRUE))
})
