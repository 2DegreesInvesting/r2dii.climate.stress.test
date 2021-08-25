test_that("without specified arguments, read_price_data() throws error", {
  testthat::expect_error(
    read_price_data(),
    "Must provide 'path'"
  )
})

test_that("with missing argument for version, read_price_data() throws error", {
  test_data_prices <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
  )

  test_data_prices %>%
    readr::write_csv(file.path(tempdir(), "prices_data_input.csv"))

  test_input_path <- file.path(tempdir(), "prices_data_input.csv")

  testthat::expect_error(
    read_price_data(path = test_input_path),
    "Must provide 'version'"
  )

  unlink(file.path(tempdir(), "prices_data_input.csv"))
})

test_that("with valid arguments set, read_price_data() returns data.frame with
          both old and new versions", {
  test_data_prices_new <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
  )

  test_data_prices_new %>%
    readr::write_csv(file.path(tempdir(), "new_prices_data_input.csv"))

  test_input_path_new <- file.path(tempdir(), "new_prices_data_input.csv")

  test_version <- "new"

  test_data_new <- read_price_data(
    path = test_input_path_new,
    version = test_version
  )

  testthat::expect_s3_class(test_data_new, "data.frame")

  unlink(file.path(tempdir(), "new_prices_data_input.csv"))

  test_data_prices_old <- tibble::tribble(
    ~year, ~sector, ~technology, ~sector_unit_ds, ~price_unit_iea, ~price_unit_etr, ~B2DS, ~b2ds_source, ~NPS, ~nps_source, ~SDS, ~sds_source, ~Baseline, ~baseline_source,
    2020, "Power", "HydroCap", "MW", "Dollars per MwH", "USD per MWh", NA_real_, "test_source", 10, "test_source", 10, "test_source", 10, "custom",
    2030, "Coal", "Coal", "TPA", "US$2010/GJ", "Dollars per ton", 10, "test_source", 10, "test_source", 10, "test_source", 10, "custom"
  )

  test_data_prices_old %>%
    readr::write_csv(file.path(tempdir(), "old_prices_data_input.csv"))

  test_input_path_old <- file.path(tempdir(), "old_prices_data_input.csv")

  test_version <- "old"

  test_data_old <- read_price_data(
    path = test_input_path_old,
    version = test_version
  )

  testthat::expect_s3_class(test_data_old, "data.frame")

  unlink(file.path(tempdir(), "old_prices_data_input.csv"))
})

test_that("with invalid argument for version, read_price_data() returns throws
          a helpful error", {
  test_data_prices_new <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
  )

  test_data_prices_new %>%
    readr::write_csv(file.path(tempdir(), "new_prices_data_input_invalid.csv"))

  test_input_path_new <- file.path(tempdir(), "new_prices_data_input_invalid.csv")

  test_version <- "invalid"

  testthat::expect_error(
    read_price_data(
      path = test_input_path_new,
      version = test_version
    ),
    "version_allowed is not TRUE"
  )

  unlink(file.path(tempdir(), "new_prices_data_input_invalid.csv"))
})

