test_that("company with positive exposure and production value is not removed", {
  test_data <- tibble::tribble(
    ~year, ~investor_name, ~portfolio_name, ~ald_sector, ~technology, ~scenario, ~scenario_geography, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
    2021, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
    2022, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
    2023, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
    2024, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
    2025, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 1, 0.01,
  )
  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  not_removed <- test_data %>%
    remove_sectors_with_missing_production(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(not_removed), nrow(test_data))

  unlink(test_log_path)
})

test_that("company with positive exposure and zero production value is removed", {
  test_data <- tibble::tribble(
    ~year, ~investor_name, ~portfolio_name, ~ald_sector, ~technology, ~scenario, ~scenario_geography, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
    2021, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
    2022, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
    2023, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
    2024, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
    2025, "investor", "portfolio", "Automotive", "Electric", "scenario_a", "Global", "company_x", 0, 0.01,
  )

  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  removed <- test_data %>%
    remove_sectors_with_missing_production(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(removed), 0)

  unlink(test_log_path)
})
