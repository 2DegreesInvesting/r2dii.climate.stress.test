#' Read in scenario data
#'
#' Function reads in scenario data and checks for existence of required columns.
#'
#'
#' @return A tibble holding scenario data.
read_scenario_data <- function() {
  scenario_data <- STDataMGMT::Scenarios_AnalysisInput

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
