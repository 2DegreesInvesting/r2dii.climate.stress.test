test_that("calculate_net_profits penalizes companies for late build out of low
          carbon technologies", {
  input_data <- tibble::tribble(
    ~company_name, ~baseline, ~late_sudden, ~Baseline_price, ~late_sudden_price, ~net_profit_margin, ~direction, ~proximity_to_target, ~scenario_geography, ~year, ~emission_factor,
    "leader", 100, 150, 10, 10, 0.1, "increasing", 1, "Global", 2030, 1,
    "laggard", 100, 150, 10, 10, 0.1, "increasing", 0.5, "Global", 2030, 1
  )

  carbon_data_test <- tibble::tribble(
    ~year, ~model, ~scenario, ~variable, ~unit, ~carbon_tax, ~scenario_geography,
    2030, "MESSAGEix-GLOBIOM 1.0", "Current policies (Hot house world, Rep)", "Price|Carbon", 10, 10, "Global"
  )


  test_shock_year <- 2021
  test_end_year_lookup <- 2040

  net_profits <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    end_year = test_end_year_lookup
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
    2030, "MESSAGEix-GLOBIOM 1.0", "Current policies (Hot house world, Rep)", "Price|Carbon", 10, 10, "Global"
  )


  test_shock_year <- 2021
  test_end_year_lookup <- 2040

  net_profits <- calculate_net_profits(input_data,
    carbon_data = carbon_data_test,
    shock_year = test_shock_year,
    end_year = test_end_year_lookup
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
