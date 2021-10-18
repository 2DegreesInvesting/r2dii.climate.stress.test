# validate_input_values ---------------------------------------------------
test_that("Error is thrown if an input value is out of bounds", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    terminal_value = 0,
    risk_free_rate = -1,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    company_exclusion = TRUE
  ), "risk_free_rate")
})

test_that("No rrror is thrown if an input value equals a bound", {
  expect_null(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    terminal_value = 0,
    risk_free_rate = min(risk_free_rate_range_lookup),
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    company_exclusion = TRUE
  ))
})
