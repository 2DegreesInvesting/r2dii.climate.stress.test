test_that("without specified arguments, read_pacta_results throws error", {
  testthat::expect_error(
    read_ngfs_carbon_tax(),
    "Must provide 'path'"
  )
})

test_that("with all arguments set, read_pacta_results returns data.frame", {
  test_path <- testthat::test_path("test_data", "ngfs_carbon_tax.csv")
  test_data <- read_ngfs_carbon_tax(
    path = test_path
  )

  testthat::expect_s3_class(test_data, "data.frame")
})
