#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors
#' @param asset_type A character vector of length 1 that indicates the type of
#'   asset to be read in. Wrangling steps vary by asset, so this is essential.
#'   Allowed values are "equity", "bonds", "loans".
#' @param level A character vector of length 1 that indicates whether results on
#'   the company level or on the portfolio level are to be analyzed.
#'   Allowed values are "company", "portfolio".
#'
#' @family import functions
#'
#' @export
read_pacta_results <- function(path = NULL,
                               asset_type = NULL,
                               level = NULL) {
  path %||% stop("Must provide 'path'")
  asset_type %||% stop("Must provide 'asset_type'")
  level %||% stop("Must provide 'level'")

  level_allowed <- level %in% c("company", "portfolio")
  stopifnot(level_allowed)

  asset_type_allowed <- asset_type %in% c("equity", "bonds", "loans")
  stopifnot(asset_type_allowed)

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  if (asset_type %in% c("equity", "bonds")) {
    data <- readRDS(path)

    expected_columns <- c(
      "investor_name", "portfolio_name", "scenario", "allocation",
      "equity_market", "scenario_geography", "year", "ald_sector",
      "technology", "plan_tech_prod", "plan_carsten", "scen_tech_prod",
      "plan_sec_prod", "plan_sec_carsten"
    )

    if (identical(level, "company")) {
      expected_columns <- c(expected_columns, "id", "company_name")
    }

    data_has_expected_columns <- all(expected_columns %in% colnames(data))
    stopifnot(data_has_expected_columns)

    data <- data %>%
      dplyr::select(.env$expected_columns)

    output_has_expected_columns <- all(expected_columns %in% colnames(data))
    stopifnot(output_has_expected_columns)
  } else {
    data <- readr::read_csv(
      path,
      col_types = readr::cols(
        sector = "c",
        technology = "c",
        year = "d",
        region = "c",
        scenario_source = "c",
        name_ald = "c",
        metric = "c",
        production_weighted = "d",
        technology_share = "d",
        production_unweighted = "d",
        loan_share_outstanding = "d",
        loan_share_credit_limit = "d"
      )
    )

    expected_columns <- c(
      "sector", "technology", "year", "region", "scenario_source", "name_ald",
      "metric", "production_weighted", "technology_share", "production_unweighted",
      "loan_share_outstanding", "loan_share_credit_limit"
    )

    data_has_expected_columns <- all(expected_columns %in% colnames(data))
    stopifnot(data_has_expected_columns)
  }

  return(data)
}
