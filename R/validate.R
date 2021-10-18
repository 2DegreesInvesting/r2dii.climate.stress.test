#' Check that input values are valid
#'
#' @inheritParams main_stress_test_equity
#'
#' @return NULL
validate_input_values <- function(company_exclusion, lgd_senior_claims, lgd_subordinated_claims) {

  if (!is.logical(company_exclusion)) {
    stop("Company exclusion must be a boolean.")
  }

  if (!dplyr::between(lgd_senior_claims, min(lgd_senior_claims_range_lookup), max(lgd_senior_claims_range_lookup))) {
    stop("Argument lgd_senior_claims is outside accepted range.")
  }

  if (!dplyr::between(lgd_subordinated_claims, min(lgd_subordinated_claims_range_lookup), max(lgd_subordinated_claims_range_lookup))) {
    stop("Argument lgd_subordinated_claims is outside accepted range.")
  }

}
