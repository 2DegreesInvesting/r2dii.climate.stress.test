#' Read in scenario data
#'
#' Function reads in scenario data and checks for existence of required columns.
#'
#' @param path Path to dir holding scenario data.
#'
#' @return A tibble holding scenario data.
read_scenario_data <- function(path = NULL) {
  if(is.null(path)) {
    scenario_data <- STDataMGMT::Scenarios_AnalysisInput
  } else {
    scenario_data <- validate_file_exists(file.path(path)) %>%
      readr::read_csv(
        col_types = readr::cols(
          scenario_geography = "c",
          scenario = "c",
          scenario_type = "c",
          ald_sector = "c",
          units = "c",
          ald_business_unit = "c",
          year = "d",
          direction = "c",
          fair_share_perc = "d"
        )
      )
  }

  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c(
      "scenario_geography", "scenario", "scenario_type",
      "ald_sector", "units", "ald_business_unit", "year",
      "direction", "fair_share_perc"
    )
  )

  return(scenario_data)
}
