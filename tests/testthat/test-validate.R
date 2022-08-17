# validate_input_values ---------------------------------------------------
test_that("Error is thrown if input values are of incorrect type", {
  # logical
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = "FALSE",
    asset_type = "loans",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "logical")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = "FALSE",
    asset_type = "loans",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "logical")

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
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "numeric")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = "1",
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
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
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "numeric")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = c("1", "2"),
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
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
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "risk_free_rate")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = -1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
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
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "Invalid input: -1, 100.")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = c(-1, 0, 100),
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "Invalid input: -1, 100.")
})

test_that("Error is thrown if a character input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "derivates",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "asset_type")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "derivates",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "asset_type")

  # length > 1
  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = c("derivates", "fund"),
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "asset_type")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 1,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = c("derivates", "fund"),
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "asset_type")
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
    shock_year = 2030,
    fallback_term = 4.5,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ), "whole number")

  expect_error(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0.02,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4.5,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
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
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "trisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ))

  expect_null(validate_input_values(
    baseline_scenario = "WEO2019_SPS",
    shock_scenario = "WEO2019_SDS",
    scenario_geography = "Global",
    lgd = 0.45,
    risk_free_rate = 0,
    discount_rate = 0.07,
    growth_rate = 0.06,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans",
    risk_type = "lrisk",
    settlement_factor = 1,
    scc = 40,
    exp_share_damages_paid = 0.027
  ))
})
