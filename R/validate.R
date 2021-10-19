#' Check that input values are valid
#'
#' Checks that user inputs are within defined ranges.
#'
#' @inheritParams run_stress_test_loans
#'
#' @return NULL
validate_input_values <- function(lgd_senior_claims, lgd_subordinated_claims,
                                  terminal_value, risk_free_rate, discount_rate,
                                  div_netprofit_prop_coef, company_exclusion,
                                  credit_type = NULL) {
  if (!is.logical(company_exclusion)) {
    stop("Company exclusion must be a boolean.")
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
    stop("Argument div_netprofit_prop_coef_range_lookup is outside accepted range.")
  }

  if (!is.null(credit_type) && !credit_type %in% credit_type_lookup) {
    stop("Argument credit type does not hold an accepted value.")
  }
}
