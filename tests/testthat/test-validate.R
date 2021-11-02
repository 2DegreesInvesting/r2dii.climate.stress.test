# validate_input_values ---------------------------------------------------
test_that("Error is thrown if an input value is out of bounds", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    terminal_value = 0,
    risk_free_rate = -1,
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "risk_free_rate")
})

test_that("Error is thrown if term is not an integer", {
  expect_error(validate_input_values(
    lgd_senior_claims = 0.45,
    lgd_subordinated_claims = 0.75,
    terminal_value = 0,
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
    terminal_value = 0,
    risk_free_rate = min(risk_free_rate_range_lookup),
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
    terminal_value = 0,
    risk_free_rate = min(risk_free_rate_range_lookup),
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
    lgd_subordinated_claims = 0.75,
    terminal_value = c(0, 1),
    risk_free_rate = min(risk_free_rate_range_lookup),
    discount_rate = 0.02,
    div_netprofit_prop_coef = 1,
    shock_year = 2030,
    term = 4,
    company_exclusion = TRUE,
    asset_type = "loans"
  ), "length 1")
})

# validate_data_has_expected_cols -----------------------------------------
test_that("No error is thrown if not colnames are missing", {
  expect_silent(validate_data_has_expected_cols(
    data = tibble::tibble(A = 1, B = 2),
    expected_columns = c("A")
  ))
})

test_that("Error is thrown if colnames are missing", {
  expect_error(
    validate_data_has_expected_cols(
      data = tibble::tibble(A = 1, B = 2),
      expected_columns = c("D", "E")
    ),
    "columns: D, E."
  )
})

