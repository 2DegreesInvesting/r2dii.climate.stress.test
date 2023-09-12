test_that("calculate_net_profits penalizes companies for late build out of low
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~overshoot_direction, ~proximity_to_target, ~scenario_geography, ~year, ~emission_factor,
    "leader", 100, 150, 10, 10, 0.1, "Increasing", 1, "Global", 2030, 1,
    "laggard", 100, 150, 10, 10, 0.1, "Increasing", 0.5, "Global", 2030, 1
  )

#verify that the proximity to target value is not related to overshoot

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax, ~scenario_geography,
    2030, "GCAM 5.3+ NGFS", "NDC", "Price|Carbon", 10, 10, "Global"
  )


  test_shock_year <- 2021
  test_market_passthrough <- 0


  net_profits <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    market_passthrough = test_market_passthrough
  )

  net_profits_baseline_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_climate_leader,
    net_profits_baseline_climate_laggard
  )
  testthat::expect_gt(
    net_profits_late_sudden_climate_leader,
    net_profits_late_sudden_climate_laggard
  )
})

test_that("calculate_net_profits does not apply penalty on lost profits for high
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~scenario_geography, ~year, ~emission_factor,
    "leader", 100, 50, 10, 10, 0.1, "declining", 1, "Global", 2030, 1,
    "laggard", 100, 50, 10, 10, 0.1, "declining", 0.5, "Global", 2030, 1
  )

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax, ~scenario_geography,
    2030, "GCAM 5.3+ NGFS", "NDC", "Price|Carbon", 10, 10, "Global"
  )


  test_shock_year <- 2021
  test_market_passthrough <- 0


  net_profits <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    market_passthrough = test_market_passthrough
  )

  net_profits_baseline_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_climate_leader,
    net_profits_baseline_climate_laggard
  )
  testthat::expect_equal(
    net_profits_late_sudden_climate_leader,
    net_profits_late_sudden_climate_laggard
  )
})

test_that("calculate_net_profits does not apply carbon tax on high
          carbon technologies before shock year", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~year, ~emission_factor,
    "high carbon technology after shock year", 100, 50, 10, 10, 0.1, "declining", 0, 2030, 1
  )

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax,
    2030, "GCAM 5.3+ NGFS", "NDC", "Price|Carbon", 10, 10,
  )


  test_shock_year_early <- 2025
  test_shock_year_late <- 2035
  test_market_passthrough <- 0



  net_profits_early <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year_early,
    market_passthrough = test_market_passthrough
  )

  net_profits_late <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year_late,
    market_passthrough = test_market_passthrough
  )

  net_profits_late_sudden_high_carbon_technology_early <- net_profits_early %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_high_carbon_technology_late <- net_profits_late %>%
    dplyr::pull(.data$net_profits_ls)


  testthat::expect_gt(
    net_profits_late_sudden_high_carbon_technology_late,
    net_profits_late_sudden_high_carbon_technology_early
  )
})


test_that("calculate_net_profits does not apply carbon tax for low
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~year, ~emission_factor,
    "high carbon technology", 100, 50, 10, 10, 0.1, "increasing", 0, 2030, 1,
    "low carbon technology", 100, 50, 10, 10, 0.1, "declining", 0, 2030, 1
  )

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax,
    2030, "GCAM 5.3+ NGFS", "NDC", "Price|Carbon", 10, 10,
  )


  test_shock_year <- 2021
  test_market_passthrough <- 0


  net_profits <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    market_passthrough = test_market_passthrough
  )

  net_profits_baseline_high_carbon_technology <- net_profits %>%
    dplyr::filter(.data$company_name == "high carbon technology") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_low_carbon_technology <- net_profits %>%
    dplyr::filter(.data$company_name == "low carbon technology") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_high_carbon_technology <- net_profits %>%
    dplyr::filter(.data$company_name == "high carbon technology") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_low_carbon_technology <- net_profits %>%
    dplyr::filter(.data$company_name == "low carbon technology") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_high_carbon_technology,
    net_profits_baseline_low_carbon_technology
  )
  testthat::expect_gt(
    net_profits_late_sudden_high_carbon_technology,
    net_profits_late_sudden_low_carbon_technology
  )
})


test_that("calculate_net_profits penalizes companies for late build out of low
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~year,
    "leader", 100, 150, 10, 10, 0.1, "increasing", 1, 2030,
    "laggard", 100, 150, 10, 10, 0.1, "increasing", 0.5, 2030,
  )


  test_shock_year <- 2021

  net_profits <- calculate_net_profits_without_carbon_tax(input_data)

  net_profits_baseline_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_baseline_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_baseline)

  net_profits_late_sudden_climate_leader <- net_profits %>%
    dplyr::filter(.data$company_name == "leader") %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_late_sudden_climate_laggard <- net_profits %>%
    dplyr::filter(.data$company_name == "laggard") %>%
    dplyr::pull(.data$net_profits_ls)

  testthat::expect_equal(
    net_profits_baseline_climate_leader,
    net_profits_baseline_climate_laggard
  )
  testthat::expect_gt(
    net_profits_late_sudden_climate_leader,
    net_profits_late_sudden_climate_laggard
  )
})



test_that("a higher market passthrough has a weaker impact on a company's net profits", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~year, ~emission_factor,
    "high carbon technology after shock year", 100, 50, 10, 10, 0.1, "declining", 0, 2030, 1
  )

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax,
    2030, "GCAM 5.3+ NGFS", "NDC", "Price|Carbon", 10, 10,
  )


  test_shock_year <- 2025
  test_market_passthrough_low <- 0
  test_market_passthrough_high <- 1



  net_profits_low_market_power <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    market_passthrough = test_market_passthrough_low
  )

  net_profits_high_market_power <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    market_passthrough = test_market_passthrough_high
  )

  net_profits_low_market_power <- net_profits_low_market_power %>%
    dplyr::pull(.data$net_profits_ls)

  net_profits_high_market_power <- net_profits_high_market_power %>%
    dplyr::pull(.data$net_profits_ls)


  testthat::expect_gt(
    net_profits_high_market_power,
    net_profits_low_market_power
  )
})
