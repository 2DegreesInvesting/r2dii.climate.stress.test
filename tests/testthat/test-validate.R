# validate_input_values ---------------------------------------------------
test_that("Error is thrown if input values are of incorrect type", {
  # logical
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = "FALSE",
    asset_type = "loans"
  ), "logical")

  # numeric
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.07,
    div_netprofit_prop_coef = "1",
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds"
  ), "numeric")
})

test_that("Error is thrown if input values are of incorrect type for input values of length > 1", {
  # numeric
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.07,
    div_netprofit_prop_coef = c("1", "2"),
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "bonds"
  ), "numeric")
})


test_that("Error is thrown if an input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = -1,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans"
  ), "risk_free_rate")

  # length > 1
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = c(-1, 0, 100),
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans"
  ), "Invalid input: -1, 100.")
})

test_that("Error is thrown if a character input value is out of bounds", {

  # length = 1
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 1,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "derivates"
  ), "asset_type")

  # length > 1
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 1,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = c("derivates", "fund")
  ), "asset_type")
})

test_that("Error is thrown if term is not an integer", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0.02,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4.5,
    use_company_terms = FALSE,
    asset_type = "loans"
  ), "whole number")
})

test_that("No error is thrown if an input value equals a bound", {
  expect_null(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.07,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    fallback_term = 4,
    use_company_terms = FALSE,
    asset_type = "loans"
  ))
})
