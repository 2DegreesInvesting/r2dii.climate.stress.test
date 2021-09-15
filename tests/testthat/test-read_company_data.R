test_that("without specified arguments, read_company_data throws error", {
  testthat::expect_error(
    read_company_data(),
    "Must provide 'path'"
  )
})

test_that("with all arguments set, read_company_data returns data.frame", {
  test_path <- testthat::test_path("test_data", "company_financial_and_production.rda")

  test_data <- read_company_data(
    path = test_path,
    asset_type = "bonds"
  )

  testthat::expect_s3_class(test_data, "data.frame")
})
