# check_valid_financial_data_values ---------------------------------------
test_that("function detects values out of range", {
  fin_data <- tibble::tibble(
    company_name = c("firm", "firm_b", "company"),
    company_id = c(1, 2, 3),
    pd = c(-1, 0, 2)
  )

  expect_error(check_valid_financial_data_values(
    financial_data = fin_data,
    asset_type = "equity"
  ), "pd detected")
})


# select_sector_scenario_combinations -------------------------------------
test_that("Error is thrown if scenarios are missing", {
  expect_error(select_sector_scenario_combinations(pacta_results = tibble::tibble(
    ald_sector = c("Automotive", "Automotive", "Power", "Power"),
    scenario = c("ETP2017_SDS", "ETP2017_ABC", "WEO2019_ABC", "WEO2019_SDS")
  )), "missing")
})

test_that("Only required sectors and scenarios are kept", {
  pacta_results <- tibble::tibble(
    ald_sector = c("Automotive", "Automotive", "Automotive", "Aviation", NA),
    scenario = c("ETP2017_SDS", "ETP2017_NPS", "WEO2019_ABC", "ETP_2017_SDS", NA)
  )

  selected_pacta_results <- select_sector_scenario_combinations(pacta_results = pacta_results)

  expect_equal(
    pacta_results %>%
      dplyr::filter(ald_sector == "Automotive" & scenario != "WEO2019_ABC"),
    selected_pacta_results
  )
})

test_that("Input is returned unaltered if only required scenarios are provided for sectors", {
  pacta_results <- tibble::tibble(
    ald_sector = c("Automotive", "Automotive", "Power", "Power"),
    scenario = c("ETP2017_SDS", "ETP2017_NPS", "WEO2019_NPS", "WEO2019_SDS")
  )

  selected_pacta_results <- select_sector_scenario_combinations(pacta_results = pacta_results)

  expect_equal(pacta_results, selected_pacta_results)
})


# check_company_terms -----------------------------------------------------
test_that("Error is thrown if terms < 1 are provided", {
  expect_error(
    check_company_terms(
      tibble::tibble(term = c(0, 2))
    ), "terms below 1"
  )
})

test_that("Error is thrown if terms are provided as decimals", {
  expect_error(
    check_company_terms(
      tibble::tibble(term = c(2, 2.1))
    ), "whole number"
  )
})
