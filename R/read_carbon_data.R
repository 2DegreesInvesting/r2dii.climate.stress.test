#' Read in carbon price data from ngfs data
#'
#'
#' @param path A string that points to the location of the file containing the
#'   carbon price data.
#'
#' @family import functions
read_carbon_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(file.path(path)) %>%
    arrow::read_parquet(
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

  return(data)
}
