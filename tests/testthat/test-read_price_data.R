# read_price_data ---------------------------------------------------------
test_that("with valid arguments set, read_price_data returns
          data.frame", {
  test_data_prices <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
  )

  test_data_prices %>%
    readr::write_csv(
      file.path(tempdir(), "internal_prices_data_input.csv")
    )

  test_input_path <- file.path(
    tempdir(), "internal_prices_data_input.csv"
  )

  test_data <- read_price_data(
    path = test_input_path,
    expected_technologies = c("RenewablesCap", "Gas")
  )

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_prices_data_input.csv"))
})

# read_price_data_old -----------------------------------------------------
test_that("with valid arguments set, read_price_data_old() returns
          data.frame", {
  test_data_prices <- tibble::tribble(
    ~year, ~sector, ~technology, ~sector_unit_ds, ~price_unit_iea, ~price_unit_etr, ~B2DS, ~b2ds_source, ~NPS, ~nps_source, ~SDS, ~sds_source, ~Baseline, ~baseline_source,
    2020, "Power", "HydroCap", "MW", "Dollars per MwH", "USD per MWh", NA_real_, "test_source", 10, "test_source", 10, "test_source", 10, "custom",
    2030, "Coal", "Coal", "TPA", "US$2010/GJ", "Dollars per ton", 10, "test_source", 10, "test_source", 10, "test_source", 10, "custom"
  )

  test_data_prices %>%
    readr::write_csv(
      file.path(tempdir(), "internal_old_prices_data_input.csv")
    )

  test_input_path <- file.path(
    tempdir(), "internal_old_prices_data_input.csv"
  )

  test_data <- read_price_data_old(
    path = test_input_path,
    expected_technologies = c("Coal")
  )

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_old_prices_data_input.csv"))
})
