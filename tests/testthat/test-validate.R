# validate_input_values ---------------------------------------------------
test_that("Error is thrown if input values are of incorrect type", {
  # logical
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = "TRUE",
    asset_type = "loans"
  ), "logical")

  # numeric
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.02,
    div_netprofit_prop_coef = "1",
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "bonds"
  ), "numeric")

})


test_that("Error is thrown if an input value is out of bounds", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = -1,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "risk_free_rate")
})

test_that("Error is thrown if a character input value is out of bounds", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 1,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "derivates"
  ), "asset_type")
})

test_that("Error is thrown if term is not an integer", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0.02,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4.5,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "whole number")
})

test_that("No error is thrown if an input value equals a bound", {
  expect_null(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ))
})

test_that("Error is thrown if an input value is of length other than 1", {

  # NULL
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    risk_free_rate = 0,
    discount_rate = NULL,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "length 1")

  # length > 1
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = c(0.75, 0.76),
    risk_free_rate = 0,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "length 1")
})
