#' Check that input values are valid
#'
#' @inheritParams main_stress_test_equity
#'
#' @return NULL
validate_input_values <- function(company_exclusion) {

  if (!is.logical(company_exclusion)) {
    stop("Company exclusion must be a boolean.")
  }

}
