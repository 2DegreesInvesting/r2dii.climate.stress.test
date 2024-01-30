#' Read in power capacity factors from csv and check that all expected columns
#' are given.
#'
#' 
#' @family import functions
read_capacity_factors_power <- function() {
  data <- STDataMGMT::prewrangled_capacity_factors

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario", "scenario_geography", "ald_business_unit", "year", "capacity_factor"
    )
  )

  return(data)
}
