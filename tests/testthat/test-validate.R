# validate_input_values ---------------------------------------------------
test_that("Error is thrown if input values are of incorrect type", {

  # numeric
  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = "1",
    shock_year = 2030,
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027,
    carbon_price_model = "no_carbon_tax"
  ), "numeric")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = "1",
    shock_year = 2030,
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "numeric")
})

test_that("Error is thrown if input values are of incorrect type for input values of length > 1", {
  # numeric
  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = c("1", "2"),
    shock_year = 2030,
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027,
    carbon_price_model = "no_carbon_tax"
  ), "numeric")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = c("1", "2"),
    shock_year = 2030,
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "numeric")
})


test_that("Error is thrown if an input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = -1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027,
    carbon_price_model = "no_carbon_tax"
  ), "risk_free_rate")

  # length > 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = c(-1, 0, 100),
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027,
    carbon_price_model = "no_carbon_tax"
  ), "Invalid input: -1, 100.")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = c(-1, 0, 100),
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "Invalid input: -1, 100.")
})

test_that("Error is thrown if a character input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2000_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
     growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "trisk",
    carbon_price_model = "no_carbon_tax"
  ), "baseline")

  # length > 1
  expect_error(validate_input_values(
    baseline_scenario = c("WEO2001_STEPS", "WEO2001_APS"),
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2025,
    risk_type = "trisk",
    carbon_price_model = "no_carbon_tax"
  ), "baseline")
})

test_that("No error is thrown if an input value equals a bound", {
  expect_null(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027,
    carbon_price_model = "no_carbon_tax"
  ))

  expect_null(validate_input_values(
    baseline_scenario = "WEO2021_STEPS",
    shock_scenario = "WEO2021_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ))
})
