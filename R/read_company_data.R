#' Read in company finacial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs and company
#' production plans, aggregated to the ticker/technology/year level for
#' `asset_type` bonds and aggregated to the company/technology/year level for
#' `asset_type` equity and aggregated to the loan_taker/technology/year level
#' for `àsset_type` loans
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#' @param asset_type A string indicating if company data are for analysis for
#'   bond or equity.
#'
#' @family import functions
#'
#' @export
read_company_data <- function(path = NULL, asset_type) {
  path %||% stop("Must provide 'path'")

  if (!asset_type %in% c("bonds", "equity", "loans")) {
    stop("Invalid asset type.")
  }

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  # TODO: once the input is in long format the expected col types can be set
  data <- readRDS(path)

  expected_columns <- c(
    "company_name", "company_id", "ald_sector", "technology", "year",
    "ald_production_unit", "ald_emissions_factor_unit", "ald_emissions_factor",
    "pd", "profit_margin_preferred", "profit_margin_unpreferred", "leverage_s_avg",
    "asset_volatility_s_avg", "ald_production"
  )

  if (asset_type == "bonds") {
    expected_columns <- c(expected_columns, "corporate_bond_ticker")
  }

  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)

  return(data)
}
