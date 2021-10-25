test_that("without specified arguments, read_pacta_results throws error", {
  testthat::expect_error(
    read_pacta_results(),
    "Must provide 'path'"
  )
})

test_that("with missing argument for asset_type, read_pacta_results throws
          error", {
  test_path <- testthat::test_path("test_data", "raw_input_equity.rda")
  test_level <- "company"

  testthat::expect_error(
    read_pacta_results(
      path = test_path,
      level = test_level
    ),
    "Must provide 'asset_type'"
  )
})

test_that("with all arguments set, read_pacta_results returns data.frame", {
  test_path <- testthat::test_path("test_data", "raw_input_equity.rda")
  test_asset_type <- "equity"
  test_level <- "company"

  test_data_new <- read_pacta_results(
    path = test_path,
    asset_type = test_asset_type,
    level = test_level
  )

  testthat::expect_s3_class(test_data_new, "data.frame")

  test_path <- testthat::test_path("test_data", "raw_input_loanbook.rda")
  test_asset_type <- "loans"
  test_level <- "company"

  test_data_new <- read_pacta_results(
    path = test_path,
    asset_type = test_asset_type,
    level = test_level
  )

  testthat::expect_s3_class(test_data_new, "data.frame")
})
