#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   capacity factors
#' @param version A string indicating whether to prepare the capacity factors
#'   basedn on the old logic (2017 data set) or the new logic (long format,
#'   multiple years and scenarios). Must be either "old" or "new".
#'
#' @family import functions
#'
#' @export
read_capacity_factors <- function(path = NULL,
                                  version = NULL) {
  path %||% stop("Must provide 'path'")
  version %||% stop("Must provide 'version'")

  version_allowed <- version %in% c("old", "new")
  stopifnot(version_allowed)

  validate_file_exists(file.path(path))

  # TODO: once the input is in long format the expected col types can be set
  data <- readr::read_csv(path, col_types = readr::cols())

  if (identical(version, "old")) {
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
      dplyr::distinct(.data$technology, .data$capacityfactor_WEO_2016) %>%
      dplyr::rename(capacity_factor = .data$capacityfactor_WEO_2016) %>%
      dplyr::mutate(scenario_geography = "Global")

    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "technology", "capacity_factor", "scenario_geography"
      )
    )

  } else {
    data <- data %>%
      dplyr::select(
        .data$scenario, .data$scenario_geography, .data$technology,
        .data$year, .data$capacity_factor
      ) %>%
      dplyr::distinct_all() %>%
      dplyr::mutate(
        scenario = dplyr::if_else(.data$scenario == "SPS", "NPS", .data$scenario)
      )

    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "scenario", "scenario_geography", "technology", "year", "capacity_factor"
      )
    )
  }

  return(data)
}
