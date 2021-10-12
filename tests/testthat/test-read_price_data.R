# test read_price_data()
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

  test_technologies <- c("RenewablesCap", "Gas")

  test_data_new <- read_price_data(
    path = test_input_path_new,
    version = test_version,
    expected_technologies = test_technologies
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

  test_technologies <- c("HydroCap", "Coal")

  test_data_old <- read_price_data(
    path = test_input_path_old,
    version = test_version,
    expected_technologies = test_technologies
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

  test_technologies <- c("RenewablesCap", "Gas")

  testthat::expect_error(
    read_price_data(
      path = test_input_path_new,
      version = test_version,
      expected_technologies = test_technologies
    ),
    "version_allowed is not TRUE"
  )

  unlink(file.path(tempdir(), "new_prices_data_input_invalid.csv"))
})

test_that("with expected technologies missing from the output, read_price_data()
          returns throws a helpful error", {
  test_data_prices_new <- tibble::tribble(
    ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
    2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
    2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
  )

  test_data_prices_new %>%
    readr::write_csv(
      file.path(tempdir(), "new_prices_data_input_miss_tech.csv")
    )

  test_input_path_new <- file.path(
    tempdir(), "new_prices_data_input_miss_tech.csv"
  )

  test_version <- "new"

  test_technologies <- c("RenewablesCap", "Gas", "Coal")

  testthat::expect_error(
    read_price_data(
      path = test_input_path_new,
      version = test_version,
      expected_technologies = test_technologies
    ),
    "prices_for_all_technologies_available is not TRUE"
  )

  unlink(file.path(tempdir(), "new_prices_data_input_miss_tech.csv"))
})

# test read_price_data_internal()
test_that("with valid arguments set, read_price_data_internal() returns
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

  test_data <- read_price_data_internal(path = test_input_path)

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_prices_data_input.csv"))
})

# test read_price_data_internal_old()
test_that("with valid arguments set, read_price_data_internal_old() returns
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

  test_data <- read_price_data_internal_old(path = test_input_path)

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_old_prices_data_input.csv"))
})
