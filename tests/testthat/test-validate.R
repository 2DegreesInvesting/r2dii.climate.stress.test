# validate_input_values ---------------------------------------------------
test_that("Error is thrown if input values are of incorrect type", {
  # numeric
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = "1",
    shock_year = 2030
  ), "numeric")
})

test_that("Error is thrown if input values are of incorrect type for input values of length > 1", {
  # numeric
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = c("1", "2"),
    shock_year = 2030
  ), "numeric")
})


test_that("Error is thrown if an input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = -1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030
  ), "risk_free_rate")

  # length > 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = c(-1, 0, 100),
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030
  ), "Invalid input: -1, 100.")
})

test_that("Error is thrown if a character input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2000_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030
  ), "baseline_scenario")

  # length > 1
  expect_error(validate_input_values(
    baseline_scenario = c("WEO2019_SPS", "WEO2019_CPS"),
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030
  ), "baseline_scenario")
})

test_that("Error is thrown if term is not an integer", {
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0.02,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030.5
  ), "whole number")
})

test_that("No error is thrown if an input value equals a bound", {
  expect_null(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030
  ))
})
