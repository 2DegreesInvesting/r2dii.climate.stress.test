# set_baseline_trajectory -------------------------------------------------
test_that("set_baseline_trajectories returns a data frame", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    baseline_scenario = "NPS"
  )

  testthat::expect_s3_class(baseline_trajectory, "data.frame")
})

test_that("set_baseline_trajectories does not fully replicate indicated scenario", {
  test_data_set_baseline <- read_test_data("data_set_baseline_traj.csv")

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    baseline_scenario = "NPS"
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
        test_data_set_baseline$ald_business_unit == "Electric",
        "plan_tech_prod"
      ]
    )
  )

  baseline_trajectory <- set_baseline_trajectory(
    data = test_data_set_baseline,
    baseline_scenario = "NPS"
  )

  baseline_trajectory_forecast <- baseline_trajectory %>% head(forecast_length)

  testthat::expect_true(
    all(baseline_trajectory_forecast$plan_tech_prod == baseline_trajectory_forecast$baseline)
  )
})


test_that("set_baseline_trajectory sets baseline values to prod forecast
          for the forecast period", {
  test_data_calc_future_prod <- read_test_data("data_calc_future_prod.csv")

  # data adaptation to the function
  test_data_calc_future_prod <- test_data_calc_future_prod %>%
    dplyr::mutate(
      scenario_change = .data$scenario_change * 10,
      TEST_BASELINE = cumsum(tidyr::replace_na(.data$scenario_change, 0))
    )
  test_data_calc_future_prod[, c(
    "ald_business_unit",
    "ald_sector",
    "company_id",
    "company_name",
    "emission_factor",
    "scenario_geography"
  )] <- NA

  scen_follows_prod <- set_baseline_trajectory(
    data = test_data_calc_future_prod,
    baseline_scenario = "TEST_BASELINE"
  )

  forecast_length <- sum(!is.na(scen_follows_prod$plan_tech_prod))

  scen_follows_prod <- scen_follows_prod %>% head(forecast_length)

  testthat::expect_true(
    all(scen_follows_prod$baseline == scen_follows_prod$plan_tech_prod)
  )
})

test_that("set_baseline_trajectory sets baseline CHANGE values to prod forecast
          for the forecast period", {
  test_data_calc_future_prod <- read_test_data("data_calc_future_prod.csv")

  # data adaptation to the function
  test_data_calc_future_prod <- test_data_calc_future_prod %>%
    dplyr::mutate(
      scenario_change = .data$scenario_change * 10,
      TEST_BASELINE = cumsum(tidyr::replace_na(.data$scenario_change, 0))
    )
  test_data_calc_future_prod[, c(
    "ald_business_unit",
    "ald_sector",
    "company_id",
    "company_name",
    "emission_factor",
    "scenario_geography"
  )] <- NA

  scen_follows_change <- set_baseline_trajectory(
    test_data_calc_future_prod,
    baseline_scenario = "TEST_BASELINE"
  )

  post_forecast_length <- sum(is.na(scen_follows_change$plan_tech_prod))

  scen_follows_change <- scen_follows_change %>%
    dplyr::bind_cols(test_data_calc_future_prod %>% dplyr::select(scenario_change)) %>%
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

# filter_negative_late_and_sudden -----------------------------------------
test_that("input remains unchanged if no negative late_and_sudden levels are
          present", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "biz", "biz"),
    ald_business_unit = c("some", "other", "some", "other"),
    late_sudden = 1:4,
    some_col = rep("sth", 4)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data, filtered_data)
})

test_that("ald_business_unit x company_name combinations that hold at least 1 negative
          value on late_and_sudden are removed", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    ald_business_unit = c("some", "some", "other", "some", "other"),
    late_sudden = c(-1, 1, 1, 0, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data %>% dplyr::filter(!(company_name == "firm" & ald_business_unit == "some")), filtered_data)
})

test_that("removal works if several company_name x ald_business_unit combinations are affected", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    ald_business_unit = c("some", "some", "other", "some", "other"),
    late_sudden = c(-1, 1, -1, -1, 1),
    some_col = rep("sth", 5)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL)

  expect_equal(input_data %>% dplyr::filter(company_name == "biz" & ald_business_unit == "other"), filtered_data)
})

test_that("error is thrown if no rows remain", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    ald_business_unit = c("some", "some", "other", "some", "other"),
    late_sudden = rep(-1, 5),
    some_col = rep("sth", 5)
  )

  expect_error(testthat::expect_warning(filtered_data <- filter_negative_late_and_sudden(input_data, log_path = NULL), "Removed"), "No rows remain")
})
