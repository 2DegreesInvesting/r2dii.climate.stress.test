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

  validate_file_exists(file.path(path))

  data <- readr::read_csv(
    path,
    col_types = readr::cols_only(
      company_name = "c",
      company_id = "d",
      corporate_bond_ticker = "c",
      pd = "d",
      net_profit_margin = "d",
      debt_equity_ratio = "d",
      volatility = "d"
    )
  )

  return(data)
}
