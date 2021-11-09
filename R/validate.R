#' Check that input values are valid
#'
#' Checks that user inputs are of length 1 and within defined ranges.
#'
#' @param asset_type String holding asset_type, for allowed value compare
#'   `asset_types_lookup`.
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `lgd_senior_claims_range_lookup`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `lgd_subordinated_claims_range_lookup`.
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame. For accepted range compare `terminal_value_range_lookup`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `risk_free_rate_range_lookup`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `discount_rate_range_lookup`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `div_netprofit_prop_coef_range_lookup`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `shock_year_range_lookup`.
#' @param term Numeric. A coefficient that determines for which maturity the
#'   expected loss should be calculated in the credit risk section. For accepted
#'   range compare `term_range_lookup`.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded.
#'
#' @return NULL
validate_input_values <- function(lgd_senior_claims, lgd_subordinated_claims,
                                  terminal_value, risk_free_rate, discount_rate,
                                  div_netprofit_prop_coef, shock_year, term,
                                  company_exclusion, asset_type) {

  input_args <- list(
    lgd_senior_claims, lgd_subordinated_claims, terminal_value, risk_free_rate,
    discount_rate, div_netprofit_prop_coef, shock_year, term, company_exclusion
  )

  if (any(purrr::map_int(input_args, length) != 1)) {
    stop("Input arguments to stress test run need to be of length 1")
  }

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
    stop("Argmemnt term must be a whole number")
  }

  if (!asset_type %in% asset_types_lookup) {
    stop("Invalid value for argument asset_type")
  }
}
