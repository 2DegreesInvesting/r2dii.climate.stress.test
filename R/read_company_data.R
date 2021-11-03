#' Read in company financial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs, aggregated to the
#' ticker level for `asset_type` bonds, to the company level for `asset_type`
#' equity and to the loan_taker level for `àsset_type` loans
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#' @param asset_type A string indicating if company data are for analysis for
#'   bond or equity.
#'
#' @family import functions
#'
#' @export
read_financial_data <- function(path = NULL, asset_type) {
  path %||% stop("Must provide 'path'")

  if (!asset_type %in% c("bonds", "equity", "loans")) {
    stop("Invalid asset type.")
  }

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  data <- readr::read_rds(path)

  expected_columns <- c(
    "company_name", "company_id", "pd", "net_profit_margin", "debt_equity_ratio",
    "volatility"
  )

  if (asset_type == "bonds") {
    expected_columns <- c(expected_columns, "corporate_bond_ticker")
  }

  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  return(data)
}
