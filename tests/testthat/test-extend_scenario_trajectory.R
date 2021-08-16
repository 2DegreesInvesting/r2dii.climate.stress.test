test_that("without specified arguments, extend_scenario_trajectory throws error", {
  testthat::expect_error(
    extend_scenario_trajectory(),
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
    extend_scenario_trajectory(
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

  test_results <- extend_scenario_trajectory(
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
