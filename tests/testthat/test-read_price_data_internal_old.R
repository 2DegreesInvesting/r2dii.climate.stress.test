test_that("without specified arguments, read_price_data_internal_old() throws
          error", {
  testthat::expect_error(
    read_price_data_internal_old(),
    "argument \"path\" is missing, with no default"
  )
})

test_that("with valid arguments set, read_price_data_internal_old() returns
          data.frame", {
  test_data_prices <- tibble::tribble(
    ~year, ~sector, ~technology, ~sector_unit_ds, ~price_unit_iea, ~price_unit_etr, ~B2DS, ~b2ds_source, ~NPS, ~nps_source, ~SDS, ~sds_source, ~Baseline, ~baseline_source,
    2020, "Power", "HydroCap", "MW", "Dollars per MwH", "USD per MWh", NA_real_, "test_source", 10, "test_source", 10, "test_source", 10, "custom",
    2030, "Coal", "Coal", "TPA", "US$2010/GJ", "Dollars per ton", 10, "test_source", 10, "test_source", 10, "test_source", 10, "custom"
  )

  test_data_prices %>%
    readr::write_csv(file.path(tempdir(), "internal_old_prices_data_input.csv"))

  test_input_path <- file.path(tempdir(), "internal_old_prices_data_input.csv")

  test_data <- read_price_data_internal_old(path = test_input_path)

  testthat::expect_s3_class(test_data, "data.frame")

  unlink(file.path(tempdir(), "internal_old_prices_data_input.csv"))
})
