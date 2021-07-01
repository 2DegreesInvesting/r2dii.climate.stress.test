test_that("without specified arguments, read_capacity_factors throws
          error", {
  testthat::expect_error(
    read_capacity_factors(),
    "Must provide 'path'"
  )
})

test_that("with missing argument for version, read_capacity_factors throws
          error", {
  test_path <- testthat::test_path("test_data", "capacity_factors_new.csv")

  testthat::expect_error(
    read_capacity_factors(
      path = test_path
    ),
    "Must provide 'version'"
  )
})

test_that("with all arguments set, old and new versions return data.frame", {
  test_path_new <- testthat::test_path("test_data", "capacity_factors_new.csv")
  test_version_new <- "new"

  test_data_new <- read_capacity_factors(
    path = test_path_new,
    version = test_version_new
  )

  testthat::expect_s3_class(test_data_new, "data.frame")

  test_path_old <- testthat::test_path("test_data", "capacity_factors_old.csv")
  test_version_old <- "old"

  test_data_old <- read_capacity_factors(
    path = test_path_old,
    version = test_version_old
  )

  testthat::expect_s3_class(test_data_old, "data.frame")
})
