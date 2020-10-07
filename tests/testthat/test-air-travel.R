testthat::context("air-travel")

testthat::test_that("doubling distances results in doubled co2e", {
  e1 <- emissions("airplane", type = "distance", c(100, 1000, 10000))
  e2 <- emissions("airplane", type = "distance", 2 * c(100, 1000, 10000))
  testthat::expect_equal(2 * e1, e2)
})


