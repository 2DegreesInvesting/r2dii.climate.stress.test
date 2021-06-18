#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors
#'
#' @family import functions
#'
#' @export
read_capacity_factors <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  # TODO: once the input is in long format the expected col types can be set
  data <- readr::read_csv(path, col_types = cols())

  # FIXME: Since we only ever have one file as input so far, I am leaving the
  # years as a magic number. Once process for the raw data is documented, this
  # will be overhauled
  years <- 2015:2040
  data_has_expected_columns <- all(
    c(
      "Region", "Scenario", "Source.x", "Technology", "region_2dii", "Source.y",
      glue::glue("Cap_Y{years}"), glue::glue("Gen_Y{years}"),
      glue::glue("capacityfactor_WEO_{years}")
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  return(data)
}
