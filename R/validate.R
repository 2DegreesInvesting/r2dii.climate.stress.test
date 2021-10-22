#' Check that input values are valid
#'
#' Checks that user inputs are within defined ranges.
#'
#' @inheritParams run_stress_test_loans
#'
#' @return NULL
validate_input_values <- function(lgd_senior_claims, lgd_subordinated_claims,
                                  terminal_value, risk_free_rate, discount_rate,
                                  div_netprofit_prop_coef, shock_year, term,
                                  company_exclusion, credit_type = NULL) {
  if (!is.logical(company_exclusion)) {
    stop("Argmuent company_exclusion must be a boolean.")
  }

  if (!dplyr::between(lgd_senior_claims, min(lgd_senior_claims_range_lookup), max(lgd_senior_claims_range_lookup))) {
    stop("Argument lgd_senior_claims is outside accepted range.")
  }

  if (!dplyr::between(lgd_subordinated_claims, min(lgd_subordinated_claims_range_lookup), max(lgd_subordinated_claims_range_lookup))) {
    stop("Argument lgd_subordinated_claims is outside accepted range.")
  }

  if (!dplyr::between(terminal_value, min(terminal_value_range_lookup), max(terminal_value_range_lookup))) {
    stop("Argument terminal_value is outside accepted range.")
  }

  if (!dplyr::between(risk_free_rate, min(risk_free_rate_range_lookup), max(risk_free_rate_range_lookup))) {
    stop("Argument risk_free_rate is outside accepted range.")
  }

  if (!dplyr::between(discount_rate, min(discount_rate_range_lookup), max(discount_rate_range_lookup))) {
    stop("Argument discount_rate is outside accepted range.")
  }

  if (!dplyr::between(div_netprofit_prop_coef, min(div_netprofit_prop_coef_range_lookup), max(div_netprofit_prop_coef_range_lookup))) {
    stop("Argument div_netprofit_prop_coef is outside accepted range.")
  }

  if (!dplyr::between(shock_year, min(shock_year_range_lookup), max(shock_year_range_lookup))) {
    stop("Argument shock_year is outside accepted range.")
  }

  if (!shock_year %% 1 == 0) {
    stop("Argmuent shock_year must be a whole number")
  }

  if (!dplyr::between(term, min(term_range_lookup), max(term_range_lookup))) {
    stop("Argument term is outside accepted range.")
  }

  # ADO 1943 - Once we decide to add a separate Merton calculation on the average
  # maturity of a portfolio, this check will need to be removed
  if (!term %% 1 == 0) {
    stop("Argmuent term must be a whole number")
  }

  if (!is.null(credit_type) && !credit_type %in% credit_type_lookup) {
    stop("Argument credit_type does not hold an accepted value.")
  }
}
