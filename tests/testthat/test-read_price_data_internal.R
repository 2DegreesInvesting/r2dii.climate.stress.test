# test_that("without specified arguments, read_price_data_internal() throws
#           error", {
#   testthat::expect_error(
#     read_price_data_internal(),
#     "Must provide 'path'"
#   )
# })
#
# test_that("with valid arguments set, read_price_data_internal() returns
#           data.frame", {
#   test_data_prices <- tibble::tribble(
#     ~year, ~source, ~scenario, ~scenario_geography, ~technology, ~indicator, ~unit, ~price,
#     2020, "WEO2020", "SDS", "United States", "Gas", "price", "usd/Mbtu", 10,
#     2030, "WEO2020", "SDS", "United States", "RenewablesCap", "LCOE", "$/MWh", 10
#   )
#
#   test_data_prices %>%
#     readr::write_csv(file.path(tempdir(), "prices_data_input.csv"))
#
#   test_input_path <- file.path(tempdir(), "prices_data_input.csv")
#
#   test_data <- read_price_data_internal(path = test_input_path)
#
#   testthat::expect_s3_class(test_data, "data.frame")
#
#   unlink(file.path(tempdir(), "prices_data_input.csv"))
# })
