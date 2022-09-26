#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors.
#' @family import functions
read_capacity_factors_power <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(col_types = readr::cols())

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario", "scenario_geography", "technology", "year", "capacity_factor"
    )
  )

  return(data)
}
