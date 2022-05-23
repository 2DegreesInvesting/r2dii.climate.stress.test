#' Obtain available geographies for sector
#'
#' Function returns a vector holding names of scenario_geographies for a
#' provided `sector`. Source of truth is the `overview`
#' `scenario_geography_x_ald_sector` per default. In case `sector` is not
#' available an error is thrown.
#' @param sector String of length 1 holding sector name.
#' @param overview A tibble holding valid combinations of `scenario_geography`
#'   and `ald_sector`.
#'
#' @return A string vector holding supported scenario_geographies.
#' @export
#'
#' @examples
#' geographies_for_sector("Coal")
geographies_for_sector <- function(sector, overview = scenario_geography_x_ald_sector) {
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
    dplyr::filter(.data$ald_sector == sector) %>%
    dplyr::pull(.data$scenario_geography) %>%
    unique()

  return(valid_scenario_geographies)
}

#' Title
#'
#' @inheritParams geographies_for_sector
#' @param scenario_geography String of length 1 holding sector name.
#'
#' @return
#' @export
#'
#' @examples
scenario_x_source_for_sector_x_geography <- function(sector, scenario_geography,
                                                     overview = scenario_geography_x_ald_sector) {

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

  sector_x_geography <- overview %>%
    dplyr::filter(.data$ald_sector == sector & .data$scenario_geography == scenario_geography) %>%
    dplyr::select(.data$scenario, .data$source)

  if (nrow(sector_x_geography) > 0) {
    rlang::abort(c(
      "Provided combination of sector and scenario_geography is not available.",
      x = "Provided combination of sector and scenario_geography is not available.",
      i = "Use function `geographies_for_sector` to obtain valid combinations"
    ))
  } else {
    return(sector_x_geography)
  }
}
