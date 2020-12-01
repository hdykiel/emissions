testthat::context("electricity")

testthat::test_that("doubling values results in doubled co2e", {
  e1 <-
    emissions("electricity", list(list(2015, 2016, 2017), list(9600, 7111, 2000)), "US|MA")
  e2 <- emissions("electricity", list(
    list(2015, 2016, 2017),
    sapply(list(9600, 7111, 2000), function(x)
      2 * x, simplify = FALSE)
  ), "US|MA")
  testthat::expect_equal(2 * e1, e2)
})
