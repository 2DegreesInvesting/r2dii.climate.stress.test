test_that("without specified arguments, read_company_data throws error", {
  testthat::expect_error(
    read_financial_data(),
    "Must provide 'path'"
  )
})

test_that("with all arguments set, read_company_data returns data.frame", {
  test_path <- testthat::test_path("test_data", "company_financial_data.csv")

  test_data <- read_financial_data(path = test_path)

  testthat::expect_s3_class(test_data, "data.frame")
})
