#' Read in price data from csv and check that all expected columns are given.
#'
#' @description This function reads in price data using long file format.
#'   It is expected to work with data based on IEA WEO 2020.
#' @inheritParams read_price_data
#'
#' @export
read_price_data_internal_old <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  data <- readr::read_csv(
    path,
    col_types = readr::cols(
      year = "d",
      source = "c",
      scenario = "c",
      scenario_geography = "c",
      technology = "c",
      indicator = "c",
      unit = "c",
      price = "d"
    )
  )

  expected_columns <- c(
    "year", "source", "scenario", "scenario_geography", "technology",
    "indicator", "unit", "price"
  )

  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)

  return(data)
  # TODO: add check that looks if all technologies are given (requires new input arg)
}
