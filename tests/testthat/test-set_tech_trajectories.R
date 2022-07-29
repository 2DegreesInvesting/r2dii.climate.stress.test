# set_baseline_trajectory -------------------------------------------------
test_that("set_baseline_trajectories returns a data frame", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS"
  )

  testthat::expect_s3_class(baseline_trajectory, "data.frame")
})

test_that("set_baseline_trajectories does not fully replicate indicated scenario", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS"
  )

  testthat::expect_false(
    all(baseline_trajectory$NPS == baseline_trajectory$baseline)
  )
})

test_that("set_baseline_trajectories replicates provided production trajectory
          until end of production forecast period", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  # find number of years with provided production forecast per tech
  forecast_length <- sum(
    !is.na(
      test_data_set_baseline[
        test_data_set_baseline$technology == "Electric",
        "plan_tech_prod"
      ]
    )
  )

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    scenario_to_follow_baseline = "NPS"
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

# set_ls_trajectory -------------------------------------------------------
test_that("set_ls_trajectory returns a data frame", {
  test_data_set_late_sudden <- read_test_data("data_set_late_sudden.csv")
  test_shock_scenario <- read_test_data("shock_scenario.csv")

  ls_trajectory <- set_ls_trajectory(
    data = test_data_set_late_sudden,
    scenario_to_follow_ls = "SDS",
    shock_scenario = test_shock_scenario,
    scenario_to_follow_ls_aligned = "SDS",
    start_year = 2020,
    end_year = 2040,
    analysis_time_frame = 5,
    log_path = NULL
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
    scenario_to_follow_ls_aligned = "SDS",
    start_year = 2020,
    end_year = 2040,
    analysis_time_frame = 5,
    log_path = NULL
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
    scenario_to_follow_ls_aligned = "SDS",
    start_year = 2020,
    end_year = 2040,
    analysis_time_frame = 5,
    log_path = NULL
  ) %>%
    dplyr::filter(.data$technology == "Electric" & .data$year < year_of_shock) %>%
    dplyr::mutate(
      change_ls = round(.data$late_sudden - dplyr::lag(.data$late_sudden), 7),
      scenario_change_aligned = round(.data$scen_to_follow_aligned - dplyr::lag(.data$scen_to_follow_aligned), 7)
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


# filter_negative_late_and_sudden -----------------------------------------
test_that("input remains unchanged if no negative late_and_sudden levels are
          present", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "biz", "biz"),
    technology = c("some", "other", "some", "other"),
    late_sudden = 1:4,
    some_col = rep("sth", 4)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data, filtered_data)
})

test_that("technology x company_name combinations that hold at least 1 negative
          value on late_and_sudden are removed", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_sudden = c(-1, 1, 1, 0, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data %>% dplyr::filter(!(company_name == "firm" & technology == "some")), filtered_data)
})

test_that("removal works if several company_name x technology combinations are affected", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_sudden = c(-1, 1, -1, -1, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data %>% dplyr::filter(company_name == "biz" & technology == "other"), filtered_data)
})

test_that("error is thrown if no rows remain", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_sudden = rep(-1, 5),
    some_col = rep("sth", 5)
  )

  expect_error(testthat::expect_warning(filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL), "Removed"), "No rows remain")
})
