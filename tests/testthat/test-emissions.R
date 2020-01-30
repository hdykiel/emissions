test_that("emissions function works as expected", {
  expect_equal(emissions("plane", "distance", 1000), 0.175)
  expect_equal(emissions("plane", "spend", 500), 0.85)
  expect_error(emissions("llama", "distance", 1000))
  expect_error(emissions("plane", "number of flights", 10))
  expect_error(emissions("plane", "distance", "1000"))
})
