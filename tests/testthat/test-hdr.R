test_that("HDR", {
  hdr <- hdr_table(rnorm(10000), prob = 0.95)
  expect_equal(hdr$lower, -1.96, tolerance = 0.02)
  expect_equal(hdr$upper, 1.96, tolerance = 0.02)
  #expect_equal(hdr$mode, 0, tolerance = 0.1)
  expect_lt(abs(hdr$density - dnorm(1.96)), 0.01)
})
