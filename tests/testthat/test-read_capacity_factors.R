test_that("without specified arguments, read_capacity_factors throws
          error", {
  testthat::expect_error(
    read_capacity_factors(),
    "Must provide 'path'"
  )
})
