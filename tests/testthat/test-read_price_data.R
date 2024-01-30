# read_price_data ---------------------------------------------------------
test_that("with valid arguments set, read_price_data returns
          data.frame", {
  test_data <- read_price_data()

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_prices_data_input.csv"))
})
