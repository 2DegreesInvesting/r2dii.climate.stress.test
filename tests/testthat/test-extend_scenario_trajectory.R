test_that("company-ald_business_unit target production after extension equals SMSP for
          low carbon ald_business_unit", {
  test_data <- read_test_data("new_extend_scenario_trajectories.csv")
  test_scenario <- read_test_data("scenario_data.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5
  test_target_scenario <- "SDS"

  test_results <- extend_scenario_trajectory(
    data = test_data,
    scenario_data = test_scenario,
    start_analysis = test_start,
    end_analysis = test_end,
    time_frame = test_horizon,
    target_scenario = test_target_scenario
  )

  verify_technology <- "Electric"
  verify_sector <- "Automotive"
  verify_scenario <- "SDS"

  initial_tech_target_electric <- test_data %>%
    dplyr::filter(
      .data$year == test_start,
      .data$ald_business_unit == verify_technology
    ) %>%
    dplyr::pull(.data$plan_tech_prod)

  initial_sector_target_auto <- test_data %>%
    dplyr::filter(
      .data$year == test_start,
      .data$ald_sector == verify_sector
    ) %>%
    dplyr::group_by(
      .data$ald_sector, .data$scenario_geography
    ) %>%
    dplyr::summarise(
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$plan_sec_prod)

  verify_scenario_change_end_year_electric_target <- test_scenario %>%
    dplyr::filter(
      .data$year == test_end,
      .data$ald_business_unit == verify_technology,
      .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$fair_share_perc)

  verify_extension_end_year_electric_target <- initial_tech_target_electric +
    (initial_sector_target_auto * verify_scenario_change_end_year_electric_target)

  verify_result_electric <- test_results %>%
    dplyr::filter(
      .data$year == test_end,
      .data$ald_business_unit == verify_technology
    ) %>%
    dplyr::pull(verify_scenario)


  testthat::expect_equal(
    verify_extension_end_year_electric_target, verify_result_electric
  )
})

test_that("company-ald_business_unit target production after extension equals TMSR for
          high carbon ald_business_unit", {
  test_data <- read_test_data("new_extend_scenario_trajectories.csv")
  test_scenario <- read_test_data("scenario_data.csv")

  test_start <- 2020
  test_end <- 2040
  test_horizon <- 5
  test_target_scenario <- "SDS"

  test_results <- extend_scenario_trajectory(
    data = test_data,
    scenario_data = test_scenario,
    start_analysis = test_start,
    end_analysis = test_end,
    time_frame = test_horizon,
    target_scenario = test_target_scenario
  )

  verify_technology <- "Coal"
  verify_scenario <- "SDS"

  initial_tech_target_coal <- test_data %>%
    dplyr::filter(
      .data$year == test_start,
      .data$ald_business_unit == verify_technology
    ) %>%
    dplyr::pull(.data$plan_tech_prod)

  verify_scenario_change_end_year_coal_target <- test_scenario %>%
    dplyr::filter(
      .data$year == test_end,
      .data$ald_business_unit == verify_technology,
      .data$scenario == verify_scenario
    ) %>%
    dplyr::pull(.data$fair_share_perc)

  verify_extension_end_year_coal_target <- initial_tech_target_coal *
    (1 + verify_scenario_change_end_year_coal_target)

  verify_result_coal <- test_results %>%
    dplyr::filter(
      .data$year == test_end,
      .data$ald_business_unit == verify_technology
    ) %>%
    dplyr::pull(verify_scenario)


  testthat::expect_equal(
    verify_extension_end_year_coal_target, verify_result_coal
  )
})
