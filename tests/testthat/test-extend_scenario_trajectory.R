test_that("without specified arguments, extend_scenario_trajectory throws error", {
  testthat::expect_error(
    extend_scenario_trajectory_old(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for scenario_data, extend_scenario_trajectory
          throws error", {
  test_data <- read_test_data("extend_scenario_trajectories.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5

  testthat::expect_error(
    extend_scenario_trajectory_old(
      data = test_data,
      start_analysis = test_start,
      end_analysis = test_end,
      time_frame = test_horizon
    ),
    "Must provide input for 'scenario_data'"
  )
})

test_that("scenario data after extension equal production in start year times
          (1 + change in scenario value)", {
  test_data <- read_test_data("extend_scenario_trajectories.csv")
  test_scenario <- read_test_data("scenario_data.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5

  test_results <- extend_scenario_trajectory_old(
    data = test_data,
    scenario_data = test_scenario,
    start_analysis = test_start,
    end_analysis = test_end,
    time_frame = test_horizon
  )

  verify_technology <- "Electric"
  verify_scenario <- "SDS"

  verify_input_manual_electric <- test_data %>%
    dplyr::filter(
      .data$year == test_start &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$scen_tech_prod)

  verify_scenario_manual_electric <- test_scenario %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$fair_share_perc)

  verify_manual_electric <- verify_input_manual_electric *
    (1 + verify_scenario_manual_electric)

  verify_result_electric <- test_results %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology
    ) %>%
    dplyr::pull(verify_scenario)


  testthat::expect_equal(verify_manual_electric, verify_result_electric)
})

test_that("company-technology target production after extension equals SMSP for
          low carbon technology", {
  test_data <- read_test_data("extend_scenario_trajectories.csv")
  test_scenario <- read_test_data("scenario_data.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5
  test_baseline_scenario <- "NPS"

  test_results <- extend_scenario_trajectory(
    data = test_data,
    scenario_data = test_scenario,
    start_analysis = test_start,
    end_analysis = test_end,
    time_frame = test_horizon,
    baseline_scenario = test_baseline_scenario
  )

  verify_technology <- "Electric"
  verify_sector <- "Automotive"
  verify_scenario <- "SDS"

  initial_tech_target_electric <- test_data %>%
    dplyr::filter(
      .data$year == test_start &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$scen_tech_prod)

  initial_sector_target_auto <- test_data %>%
    dplyr::filter(
      .data$year == test_start &
        .data$ald_sector == verify_sector &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$equity_market,
      .data$ald_sector, .data$scenario, .data$allocation,
      .data$scenario_geography
    ) %>%
    dplyr::summarise(
      scen_sec_prod = sum(.data$scen_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$scen_sec_prod)

  verify_scenario_change_end_year_electric_target <- test_scenario %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$fair_share_perc)

  verify_extension_end_year_electric_target <- initial_tech_target_electric +
    (initial_sector_target_auto * verify_scenario_change_end_year_electric_target)

  verify_result_electric <- test_results %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology
    ) %>%
    dplyr::pull(verify_scenario)


  testthat::expect_equal(
    verify_extension_end_year_electric_target, verify_result_electric
  )
})

test_that("company-technology target production after extension equals TMSR for
          high carbon technology", {
  test_data <- read_test_data("extend_scenario_trajectories.csv")
  test_scenario <- read_test_data("scenario_data.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5
  test_baseline_scenario <- "NPS"

  test_results <- extend_scenario_trajectory(
    data = test_data,
    scenario_data = test_scenario,
    start_analysis = test_start,
    end_analysis = test_end,
    time_frame = test_horizon,
    baseline_scenario = test_baseline_scenario
  )

  verify_technology <- "Coal"
  verify_scenario <- "SDS"

  initial_tech_target_coal <- test_data %>%
    dplyr::filter(
      .data$year == test_start &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$scen_tech_prod)

  verify_scenario_change_end_year_coal_target <- test_scenario %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology &
        .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$fair_share_perc)

  verify_extension_end_year_coal_target <- initial_tech_target_coal *
    (1 + verify_scenario_change_end_year_coal_target)

  verify_result_coal <- test_results %>%
    dplyr::filter(
      .data$year == test_end &
        .data$technology == verify_technology
    ) %>%
    dplyr::pull(verify_scenario)


  testthat::expect_equal(
    verify_extension_end_year_coal_target, verify_result_coal
  )
})
