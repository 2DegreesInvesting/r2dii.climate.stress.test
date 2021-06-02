test_that("set_baseline_trajectories returns a data frame", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS",
    use_prod_forecasts = TRUE
  )

  testthat::expect_s3_class(baseline_trajectory, "data.frame")
})

test_that("set_baseline_trajectories fully replicates indicated scenario,
          if prod forecast is FALSE", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS",
    use_prod_forecasts = FALSE
  )

  testthat::expect_true(
      all(baseline_trajectory$NPS == baseline_trajectory$baseline)
    )
})

test_that("set_baseline_trajectories does not fully replicate indicated scenario,
          if prod forecast is TRUE", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS",
    use_prod_forecasts = TRUE
  )

  testthat::expect_false(
      all(baseline_trajectory$NPS == baseline_trajectory$baseline)
    )
})

test_that("set_baseline_trajectories replicates provided production trajectory
          until end of production forecast period, if prod forecast is TRUE", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  # find number of years with provided production forecast per tech
  forecast_length <- sum(
    !is.na(
      test_data_set_baseline[test_data_set_baseline$technology == "Electric",
                           "plan_tech_prod"]
      )
    )

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS",
    use_prod_forecasts = TRUE
  )

  baseline_trajectory_forecast <- baseline_trajectory %>% head(forecast_length)

  testthat::expect_true(
      all(baseline_trajectory_forecast$plan_tech_prod == baseline_trajectory_forecast$baseline)
    )
})


test_that("calc_future_prod_follows_scen sets baseline values to prod forecast
          for the forecast period", {
  test_data_calc_future_prod <- read_test_data("data_calc_future_prod.csv")

  scen_follows_prod <- test_data_calc_future_prod %>%
    dplyr::mutate(
      baseline = calc_future_prod_follows_scen(
        planned_prod = test_data_calc_future_prod$plan_tech_prod,
        change_scen_prod = test_data_calc_future_prod$scenario_change
      )
    )

  forecast_length <- sum(!is.na(scen_follows_prod$plan_tech_prod))

  scen_follows_prod <- scen_follows_prod %>% head(forecast_length)

  testthat::expect_true(
    all(scen_follows_prod$baseline == scen_follows_prod$plan_tech_prod)
  )
})

test_that("calc_future_prod_follows_scen sets baseline values to prod forecast
          for the forecast period", {
  test_data_calc_future_prod <- read_test_data("data_calc_future_prod.csv")

  scen_follows_change <- test_data_calc_future_prod %>%
    dplyr::mutate(
      baseline = calc_future_prod_follows_scen(
        planned_prod = test_data_calc_future_prod$plan_tech_prod,
        change_scen_prod = test_data_calc_future_prod$scenario_change
      )
    )

  post_forecast_length <- sum(is.na(scen_follows_change$plan_tech_prod))

  scen_follows_change <- scen_follows_change %>%
    dplyr::mutate(
      baseline_change = baseline - dplyr::lag(baseline),
      baseline_change = round(baseline_change, 7),
      scenario_change = round(scenario_change, 7)
    ) %>%
    tail(post_forecast_length)

  testthat::expect_true(
    all(scen_follows_change$scenario_change == scen_follows_change$baseline_change)
  )
})


test_that("set_ls_trajectory returns a data frame", {
  test_data_set_late_sudden <- read_test_data("data_set_late_sudden.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")

  ls_trajectory <- set_ls_trajectory(
    data = test_data_set_late_sudden,
    scenario_to_follow_ls = "SDS",
    shock_scenario = test_shock_scenario,
    use_production_forecasts_ls = TRUE,
    overshoot_method = TRUE,
    scenario_to_follow_ls_aligned = "SDS",
    start_year = 2020,
    end_year = 2040
  )

  testthat::expect_s3_class(ls_trajectory, "data.frame")
})

test_that("set_ls_trajectory fully replicates baseline until year before shock,
          when technology is misaligned", {
  test_data_set_late_sudden <- read_test_data("data_set_late_sudden.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")

  year_of_shock <- test_shock_scenario$year_of_shock

  ls_trajectory_pre_shock <- set_ls_trajectory(
      data = test_data_set_late_sudden,
      scenario_to_follow_ls = "SDS",
      shock_scenario = test_shock_scenario,
      use_production_forecasts_ls = TRUE,
      overshoot_method = TRUE,
      scenario_to_follow_ls_aligned = "SDS",
      start_year = 2020,
      end_year = 2040
    ) %>%
    dplyr::filter(.data$year < year_of_shock & .data$technology == "Coal")

  testthat::expect_true(
    all(ls_trajectory_pre_shock$late_sudden == ls_trajectory_pre_shock$baseline)
  )
})

test_that("when technology is aligned, set_ls_trajectory fully replicates
          baseline until end of forecast and afterwards it follows the aligned
          scenario changes until the year of shock", {
  test_data_set_late_sudden <- read_test_data("data_set_late_sudden.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")

  year_of_shock <- test_shock_scenario$year_of_shock

  ls_trajectory_pre_shock <- set_ls_trajectory(
    data = test_data_set_late_sudden,
    scenario_to_follow_ls = "SDS",
    shock_scenario = test_shock_scenario,
    use_production_forecasts_ls = TRUE,
    overshoot_method = TRUE,
    scenario_to_follow_ls_aligned = "SDS",
    start_year = 2020,
    end_year = 2040
  ) %>%
  dplyr::filter(.data$technology == "Electric" & .data$year < year_of_shock) %>%
  dplyr::mutate(
    change_ls = round(.data$late_sudden - dplyr::lag(.data$late_sudden), 7),
    scenario_change_aligned = round(.data$scenario_change_aligned, 7)
  )

  forecast_length <- sum(!is.na(ls_trajectory_pre_shock$plan_tech_prod))

  ls_start_until_prod <- ls_trajectory_pre_shock %>% head(forecast_length)

  ls_prod_until_shock <- ls_trajectory_pre_shock %>%
    tail(nrow(ls_trajectory_pre_shock) - forecast_length)

  testthat::expect_true(
    all(ls_start_until_prod$late_sudden == ls_start_until_prod$baseline) &
    all(ls_prod_until_shock$change_ls == ls_prod_until_shock$scenario_change_aligned)
  )
})

