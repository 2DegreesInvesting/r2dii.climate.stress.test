#' Read in company financial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#'
#' @family import functions
read_financial_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(file.path(path)) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        company_id = "d",
        pd = "d",
        net_profit_margin = "d",
        debt_equity_ratio = "d",
        volatility = "d"
      )
    )

  return(data)
}
