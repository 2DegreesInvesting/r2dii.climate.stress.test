#' Read in carbon price data from ngfs data
#'
#'
#'
#' @family import functions
read_carbon_data <- function() {
  data <- STDataMGMT::ngfs_carbon_price
    
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "model", "scenario",
      "scenario_geography", "variable", "unit", "carbon_tax"
    )
  )

  return(data)
}
