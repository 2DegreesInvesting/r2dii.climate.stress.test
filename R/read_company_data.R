#' Read in company finacial data processed from eikon exports and AR master data
#' that contain information on multiple credit risk inputs and company
#' production plans.
#'
#' @param path A string that points to the location of the file containing the
#'   company financial data.
#'
#' @family import functions
#'
#' @export
read_company_data <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  # TODO: once the input is in long format the expected col types can be set
  data <- readRDS(path)

  expected_columns <- c(
    "company_name", "company_id", "bloomberg_id", "corporate_bond_ticker",
    "ald_location", "country_of_domicile", "is_ultimate_listed_parent",
    "is_ultimate_parent", "parent_company_id", "ownership_level", "ald_sector",
    "technology", "year", "ald_production", "ald_production_unit",
    "ald_emissions_factor", "ald_emissions_factor_unit", "pd", "structural",
    "profit_margin_preferred", "profit_margin_unpreferred", "leverage_s_avg",
    "asset_volatility_s_avg", "asset_drift_s_avg",
    "weighted_average_cost_of_capital_percent_s_avg", "ebitda_ltm_1_usd_s_avg",
    "ebitda_margin_percent_ltm_1_s_avg", "indicator_type_pd",
    "indicator_type_structural", "indicator_type_profit_margin_preferred",
    "indicator_type_profit_margin_unpreferred", "indicator_type_leverage_s_avg",
    "indicator_type_asset_volatility_s_avg", "indicator_type_asset_drift_s_avg",
    "indicator_type_weighted_average_cost_of_capital_percent_s_avg",
    "indicator_type_ebitda_ltm_1_usd_s_avg",
    "indicator_type_ebitda_margin_percent_ltm_1_s_avg",
    "overall_data_type_pd", "overall_data_type_structural",
    "overall_data_type_profit_margin_preferred",
    "overall_data_type_profit_margin_unpreferred",
    "overall_data_type_leverage_s_avg",
    "overall_data_type_asset_volatility_s_avg",
    "overall_data_type_asset_drift_s_avg",
    "overall_data_type_weighted_average_cost_of_capital_percent_s_avg",
    "overall_data_type_ebitda_ltm_1_usd_s_avg",
    "overall_data_type_ebitda_margin_percent_ltm_1_s_avg"
  )

  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)

  return(data)
}
