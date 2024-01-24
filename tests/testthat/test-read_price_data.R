# read_price_data ---------------------------------------------------------
test_that("with valid arguments set, read_price_data returns
          data.frame", {
  test_data_prices <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~ald_business_unit, ~ald_sector, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "some_sector", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "some_sector", "LCOE", "$/MWh", 10
  )

  test_data_prices %>%
    readr::write_csv(
      file.path(tempdir(), "internal_prices_data_input.csv")
    )

  test_input_path <- file.path(
    tempdir(), "internal_prices_data_input.csv"
  )

  test_data <- read_price_data(
    path = test_input_path
  )

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_prices_data_input.csv"))
})
