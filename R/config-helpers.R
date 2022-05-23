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
      x = "Sector is of lenth > 1.",
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
    dplyr::filter(ald_sector == sector) %>%
    dplyr::pull(scenario_geography) %>%
    unique()

  return(valid_scenario_geographies)
}
