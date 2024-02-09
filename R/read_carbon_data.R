#' Read in carbon price data from ngfs data
#'
#'
#' @param path A string that points to the location of the file containing the
#'   carbon price data.
#'
#' @family import functions
read_carbon_data <- function(path = NULL) {
  if (is.null(path)) {
    data <- STDataMGMT::ngfs_carbon_price
  } else{
    data <- validate_file_exists(file.path(path)) %>%
    readr::read_csv(
      col_types = readr::cols_only(
        year = "d",
        model = "c",
        scenario = "c",
        scenario_geography = "c",
        variable = "c",
        unit = "c",
        carbon_tax = "d"
      )
    )
  }
    
    
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "model", "scenario",
      "scenario_geography", "variable", "unit", "carbon_tax"
    )
  )

  return(data)
}
