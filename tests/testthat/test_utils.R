test_that("fetch_rds() removes partial downloads on failure", {
  dir <- local_temp_dir()
  local_mocked_bindings(download_file = function(url, destfile, ...) {
    writeLines("partial", destfile)
    stop("HTTP status was '504 Gateway Timeout'")
  })
  expect_snapshot(fetch_rds("oz_books", dir = dir), error = TRUE)
  expect_length(list.files(dir), 0)
})

test_that("fetch_rds() reads a cached file without downloading", {
  dir <- local_temp_dir()
  saveRDS(1:3, file.path(dir, "oz_books.rds"))
  local_mocked_bindings(download_file = function(...) {
    stop("should not download")
  })
  expect_equal(fetch_rds("oz_books", dir = dir), 1:3)
})

test_that("fetch_rds() downloads from raw.githubusercontent.com", {
  dir <- local_temp_dir()
  local_mocked_bindings(download_file = function(url, destfile, ...) {
    saveRDS(url, destfile)
  })
  expect_equal(
    fetch_rds("oz_books", dir = dir),
    "https://raw.githubusercontent.com/robjhyndman/weird/main/data-raw/oz_books.rds"
  )
})
