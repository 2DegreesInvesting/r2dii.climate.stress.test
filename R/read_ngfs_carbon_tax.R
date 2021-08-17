#' Read in NGFS carbon tax data from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   NGFS carbon tax information
#'
#' @family import functions
#'
#' @export
read_ngfs_carbon_tax <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  data <- readr::read_csv(
    path,
    col_types = readr::cols(
      year = "d",
      model = "c",
      scenario = "c",
      scenario_geography = "c",
      variable = "c",
      unit = "c",
      carbon_tax = "d"
    )
  )

  expected_columns <- c(
    "year", "model", "scenario", "scenario_geography", "variable", "unit",
    "carbon_tax"
  )

  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)

  return(data)
}
