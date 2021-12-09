#' Read in the PACTA results from a given project and check that all expected
#' columns are given.
#'
#' @param path A string that points to the location of the file containing the
#'   pacta results
#' @param level A character vector of length 1 that indicates whether results on
#'   the company level or on the portfolio level are to be analyzed.
#'   Allowed values are "company", "portfolio".
#'
#' @family import functions
read_pacta_results <- function(path = NULL,
                               level = NULL) {
  path %||% stop("Must provide 'path'")
  level %||% stop("Must provide 'level'")

  level_allowed <- level %in% c("company", "portfolio")
  stopifnot(level_allowed)

  data <- validate_file_exists(path) %>%
    readr::read_rds()

  expected_columns <- c(
    "investor_name", "portfolio_name", "scenario", "allocation",
    "equity_market", "scenario_geography", "year", "ald_sector",
    "technology", "plan_tech_prod", "plan_carsten", "scen_tech_prod",
    "plan_sec_prod", "plan_sec_carsten"
  )

  if (identical(level, "company")) {
    expected_columns <- c(expected_columns, "id", "company_name")
  }

  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  data <- data %>%
    dplyr::select(.env$expected_columns)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  return(data)
}
