#' Read in scenario data
#'
#' Function reads in scenario data and checks for existence of required columns.
#'
#' @param path Path to dir holding scenario data.
#'
#' @return A tibble holding scenario data.
read_scenario_data <- function(path) {
  scenario_data <- readr::read_csv(path, col_types = "ccccccncn")
  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c("scenario_source", "scenario_geography", "scenario",
                         "ald_sector", "units", "technology", "year",
                         "direction", "fair_share_perc")
  )
  return(scenario_data)
}
