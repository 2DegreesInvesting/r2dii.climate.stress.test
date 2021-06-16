test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    calculate_pd_change_annual(),
    "argument \"data\" is missing"
  )
})
