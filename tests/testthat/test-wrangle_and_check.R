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
    sector = c("Automotive", "Automotive", "Power", "Power"),
    scenario = c("ETP2017_SDS", "ETP2017_ABC", "WEO2019_NPS", "WEO2019_SDS")
  )), "missing")
})

test_that("Only required scenarios are kept", {
  pacta_results <- tibble::tibble(
    sector = c("Automotive", "Automotive", "Automotive"),
    scenario = c("ETP2017_SDS", "ETP2017_NPS", "WEO2019_ABC")
  )

  selected_pacta_results <- select_sector_scenario_combinations(pacta_results = pacta_results)

  expect_equal(
    pacta_results %>%
      dplyr::filter(scenario != "WEO2019_ABC"),
    selected_pacta_results
  )
})

test_that("Input is returned unaltered if only required scenarios are provided for sectors", {
  pacta_results <- tibble::tibble(
    sector = c("Automotive", "Automotive", "Power", "Power"),
    scenario = c("ETP2017_SDS", "ETP2017_NPS", "WEO2019_NPS", "WEO2019_SDS")
  )

  selected_pacta_results <- select_sector_scenario_combinations(pacta_results = pacta_results)

  expect_equal(pacta_results, selected_pacta_results)
})
