test_that("without specified arguments, read_capacity_factors throws
          error", {
  testthat::expect_error(
    read_capacity_factors_power(),
    "Must provide 'path'"
  )
})

test_that("with all arguments set, old and new versions return data.frame", {
  test_path_new <- testthat::test_path("test_data", "capacity_factors_new.csv")

  test_data_new <- read_capacity_factors_power(
    path = test_path_new
  )

  testthat::expect_s3_class(test_data_new, "data.frame")

  test_path_old <- testthat::test_path("test_data", "capacity_factors_old.csv")

  test_data_old <- read_capacity_factors_old(
    path = test_path_old
  )

  testthat::expect_s3_class(test_data_old, "data.frame")
})
