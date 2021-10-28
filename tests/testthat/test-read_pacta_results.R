test_that("without specified arguments, read_pacta_results throws error", {
  testthat::expect_error(
    read_pacta_results(),
    "Must provide 'path'"
  )
})

test_that("with missing argument for level, read_pacta_results throws error", {
  test_path <- testthat::test_path("test_data", "raw_input_equity.rda")

  testthat::expect_error(
    read_pacta_results(
      path = test_path
    ),
    "Must provide 'level'"
  )
})

test_that("with all arguments set, read_pacta_results returns data.frame", {
  test_path <- testthat::test_path("test_data", "raw_input_equity.rda")
  test_level <- "company"

  test_data_new <- read_pacta_results(
    path = test_path,
    level = test_level
  )

  testthat::expect_s3_class(test_data_new, "data.frame")
})
