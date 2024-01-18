
get_scenario_geography_x_ald_sector <- function(st_input_folder){

}


#' Obtain available geographies for sector
#'
#' Function returns a vector holding names of scenario_geographies for a
#' provided `sector`. Source of truth is the `overview`
#' `scenario_geography_x_ald_sector` per default. In case `sector` is not
#' available an error is thrown.
#' @param st_input_folder path to the input folder containing stress test input files
#' @param sector String of length 1 holding sector name.
#'
#' @return A string vector holding supported scenario_geographies.
#' @export
#'
#' @examples
#' geographies_for_sector("Coal")
geographies_for_sector <- function(st_input_folder, sector) {
  data <- st_read_agnostic(st_input_folder, risk_type = "trisk")


  production_data_available <- data$production_data %>%
    dplyr::distinct(.data$ald_sector, .data$ald_business_unit, .data$scenario_geography)

  scenario_data_available <-

  overview <- scenario_geography_x_ald_sector

  if (length(sector) > 1) {
    rlang::abort(c(
      "Sector must be of length 1.",
      x = "Sector is of length > 1.",
      i = "Did you set argument `sector` correctly?"
    ))
  }

  valid_sectors <- unique(overview$ald_sector)
  valid_sectors_collapsed <- glue::glue_collapse(sort(valid_sectors), sep = ", ")

  if (!sector %in% valid_sectors) {
    rlang::abort(c(
      "Provided invalid `sector`.",
      x = glue::glue("Valid sectors are: {valid_sectors_collapsed}."),
      i = "Did you set argument `sector` correctly?"
    ))
  }

  valid_scenario_geographies <- overview %>%
    dplyr::filter(.data$ald_sector == !!sector) %>%
    dplyr::pull(.data$scenario_geography) %>%
    unique()

  return(valid_scenario_geographies)
}

#' Obtain available scenario_x_source for geography - sector combinations
#'
#' Function returns a vector holding names of scenarios for a provided `sector`
#' and `scenario_geography`. To identify valid `sector` x `scenario_geography`
#' combinations refer to [geographies_for_sector()]. Source of truth is the
#' `overview` `scenario_geography_x_ald_sector` per default. In case combination
#' of `sector` and `scenario_geography` is not available an error is thrown.
#'
#' @inheritParams geographies_for_sector
#' @param scenario_geography String of length 1 holding sector name.
#'
#' @return A string holding valid scenario names.
#' @export
#'
#' @examples scenario_for_sector_x_geography("Coal", "Europe")
scenario_for_sector_x_geography <- function(st_input_folder, scenario_geography) {

  overview <- scenario_geography_x_ald_sector

  if (length(sector) > 1) {
    rlang::abort(c(
      "Sector must be of length 1.",
      x = "Sector is of length > 1.",
      i = "Did you set argument `sector` correctly?"
    ))
  }

  valid_sectors <- unique(overview$ald_sector)
  valid_sectors_collapsed <- glue::glue_collapse(sort(valid_sectors), sep = ", ")

  if (!sector %in% valid_sectors) {
    rlang::abort(c(
      "Provided invalid `sector`.",
      x = glue::glue("Valid sectors are: {valid_sectors_collapsed}."),
      i = "Did you set argument `sector` correctly?"
    ))
  }

  if (length(scenario_geography) > 1) {
    rlang::abort(c(
      "Scenario_geography must be of length 1.",
      x = "Scenario_geography is of length > 1.",
      i = "Did you set argument `scenario_geography` correctly?"
    ))
  }

  valid_scenario_geography <- unique(overview$scenario_geography)
  valid_scenario_geography_collapsed <- glue::glue_collapse(sort(valid_scenario_geography), sep = ", ")

  if (!scenario_geography %in% valid_scenario_geography) {
    rlang::abort(c(
      "Provided invalid `scenario_geography`.",
      x = glue::glue("Valid scenario_geography are: {valid_scenario_geography_collapsed}."),
      i = "Did you set argument `scenario_geography` correctly?"
    ))
  }

  valid_scenario <- overview %>%
    dplyr::filter(.data$ald_sector == !!sector & .data$scenario_geography == !!scenario_geography) %>%
    dplyr::pull(.data$scenario)

  if (length(valid_scenario) == 0) {
    rlang::abort(c(
      "Provided combination of sector and scenario_geography is not available.",
      x = "Provided combination of sector and scenario_geography is not available.",
      i = "Use function `geographies_for_sector` to obtain valid combinations"
    ))
  } else {
    return(valid_scenario)
  }
}
