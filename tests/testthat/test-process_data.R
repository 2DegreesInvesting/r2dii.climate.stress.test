test_that("company with positive exposure and production value in final year is not removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2021, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2022, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2023, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2024, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2025, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
  )
  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  not_removed <- test_data %>%
    remove_sectors_with_missing_production_end_of_forecast(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(not_removed), nrow(test_data))

  unlink(test_log_path)
})

test_that("company with positive exposure and zero production value in final year is removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
    2021, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
    2022, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
    2023, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
    2024, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
    2025, "Automotive", "Electric", "scenario_a", "company_x", 0, 0.01,
  )

  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  removed <- test_data %>%
    remove_sectors_with_missing_production_end_of_forecast(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(removed), 0)

  unlink(test_log_path)
})

test_that("company with positive production value in the start year in at least
          one technology of a sector is not removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Automotive", "Electric", "scenario_a", "company_x", 0, 0,
    2021, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2022, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2023, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2024, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2025, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2020, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2021, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2022, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2023, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2024, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2025, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01
  )
  test_start_year <- 2020
  test_log_path <- file.path(tempdir(), "log.txt")

  not_removed <- test_data %>%
    remove_sectors_with_missing_production_start_year(
      start_year = test_start_year,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(not_removed), nrow(test_data))

  unlink(test_log_path)
})

test_that("company with zero production value in the start year across all given
          technologies of a sector is removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Automotive", "Electric", "scenario_a", "company_x", 0, 0,
    2021, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2022, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2023, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2024, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2025, "Automotive", "Electric", "scenario_a", "company_x", 1, 0.01,
    2020, "Automotive", "ICE", "scenario_a", "company_x", 0, 0,
    2021, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2022, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2023, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2024, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01,
    2025, "Automotive", "ICE", "scenario_a", "company_x", 1, 0.01
  )

  test_start_year <- 2020
  test_log_path <- file.path(tempdir(), "log.txt")

  removed <- test_data %>%
    remove_sectors_with_missing_production_start_year(
      start_year = test_start_year,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(removed), 0)

  unlink(test_log_path)
})

test_that("company-tech combination with 0 production in a low carbon technology
          over entire timeframe is not removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
    2021, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
    2022, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
    2023, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
    2024, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
    2025, "Power", "NuclearCap", "scenario_a", "company_x", 0, 0.01,
  )
  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  not_removed <- test_data %>%
    remove_high_carbon_tech_with_missing_production(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(not_removed), nrow(test_data))

  unlink(test_log_path)
})

test_that("company-tech combination with positive production in a high carbon
          technology over parts of the timeframe is not removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2021, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2022, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2023, "Power", "OilCap", "scenario_a", "company_x", 1, 0.01,
    2024, "Power", "OilCap", "scenario_a", "company_x", 1, 0.01,
    2025, "Power", "OilCap", "scenario_a", "company_x", 1, 0.01,
  )
  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  not_removed <- test_data %>%
    remove_high_carbon_tech_with_missing_production(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(not_removed), nrow(test_data))

  unlink(test_log_path)
})

test_that("company-tech combination with 0 production in a high carbon technology
          over entire timeframe is removed", {
  test_data <- tibble::tribble(
    ~year, ~ald_sector, ~technology, ~scenario, ~company_name, ~plan_tech_prod, ~plan_carsten,
    2020, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2021, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2022, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2023, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2024, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
    2025, "Power", "OilCap", "scenario_a", "company_x", 0, 0.01,
  )

  test_start_year <- 2020
  test_time_horizon <- 5
  test_log_path <- file.path(tempdir(), "log.txt")

  removed <- test_data %>%
    remove_high_carbon_tech_with_missing_production(
      start_year = test_start_year,
      time_horizon = test_time_horizon,
      log_path = test_log_path
    )
  testthat::expect_equal(nrow(removed), 0)

  unlink(test_log_path)
})
