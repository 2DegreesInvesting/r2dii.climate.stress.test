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

#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors
#'
#' @family import functions
read_capacity_factors_old <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(col_types = readr::cols())

  # FIXME: Since we only ever have one file as input so far, I am leaving the
  # years as a magic number. Once process for the raw data is documented, this
  # will be overhauled
  years <- 2015:2040

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "Region", "Scenario", "Source.x", "Technology", "region_2dii", "Source.y",
      glue::glue("Cap_Y{years}"), glue::glue("Gen_Y{years}"),
      glue::glue("capacityfactor_WEO_{years}")
    )
  )

  data <- data %>%
    dplyr::select(
      .data$Region, .data$Technology, .data$region_2dii,
      .data$capacityfactor_WEO_2016
    ) %>%
    dplyr::rename(technology = .data$Technology) %>%
    dplyr::filter(
      !is.na(.data$capacityfactor_WEO_2016),
      .data$Region == "World" |
        (.data$technology %in% c("HydroCap", "NuclearCap", "RenewablesCap") &
          .data$Region == "OECD")
    ) %>%
    # ADO 2393 - legacy web tool version
    dplyr::distinct(.data$technology, .data$capacityfactor_WEO_2016) %>%
    dplyr::rename(capacity_factor = .data$capacityfactor_WEO_2016) %>%
    dplyr::mutate(scenario_geography = "Global")

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "technology", "capacity_factor", "scenario_geography"
    )
  )

  return(data)
}
