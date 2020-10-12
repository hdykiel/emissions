testthat::context("air-travel")

testthat::test_that("doubling distances results in doubled co2e", {

  e1 <- emissions("airplane", c(100, 1000, 10000))
  e2 <- emissions("airplane", 2 * c(100, 1000, 10000))
  testthat::expect_equal(2 * e1, e2)

  e1 <- emissions("commuter", c(2, 5, 15, 50))
  e2 <- emissions("commuter", 2 * c(2, 5, 15, 50))
  testthat::expect_equal(2 * e1, e2)

  e1 <- emissions("car", c(2, 5, 15, 50))
  e2 <- emissions("car", 2 * c(2, 5, 15, 50))
  testthat::expect_equal(2 * e1, e2)

})

testthat::test_that("rail uses defined thresholds", {

  e1 <- emissions("rail", c(2, 5, 15, 28, 32))
  e2 <- emissions("transit", c(2, 5))
  e3 <- emissions("commuter", c(15, 28)) #commuter_threshold <- 10
  e4 <- emissions("intercity", c(32)) # intercity_threshold <- 30
  testthat::expect_equal(e1, e2 + e3 + e4)

})

