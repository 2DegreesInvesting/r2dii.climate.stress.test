#' Read in company financial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#'
#' @family import functions
#'
#' @export
read_financial_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  data <- readr::read_rds(path)

  expected_columns <- c(
    "company_name", "company_id", "corporate_bond_ticker", "pd",
    "net_profit_margin", "debt_equity_ratio", "volatility"
  )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  return(data)
}
