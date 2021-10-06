#' Wrangles and checks sector exposures
#'
#' Wrangles sector exposures by removing invalid rows and rows from assets other
#' than `asset_types`. Also checks for duplicates and that more than 0 rows
#' remain.
#'
#' @param sector_exposures A tibble holding sector_exposures as calculated by
#'   PACTA.
#' @param asset_type Type of analyzed asset. Can be bonds or equity.
#'
#' @return `sector_exposures` holding only valid rows for `asset_type`
#' @export
wrangle_and_check_sector_exposures_eq_cb <- function(sector_exposures, asset_type) {

  if (!asset_type %in% c("Bonds", "Equity")) {
    stop("Can only wrangle dataset for asset types bonds and equity.")
  }

  valid_sector_exposures <- sector_exposures %>%
    dplyr::filter(valid_input) %>%
    dplyr::filter(asset_type == !!asset_type) %>%
    dplyr::select(-valid_input, -asset_type)

  if (nrow(valid_sector_exposures) == 0) {
    stop("No valid sector exposures available.")
  }

  report_all_duplicate_kinds(valid_sector_exposures, cuc_sector_exposures_eq_cb)

  return(valid_sector_exposures)
}


wrangle_and_check_pacta_results_eq_cb <- function(pacta_results, start_year, time_horizon, scenario_geography_filter, scenarios_filter, equity_market_filter) {

  wrangled_pacta_results <- pacta_results %>%
    dplyr::filter(!is.na(.data$scenario)) %>%
    check_scenario_settings(scenario_selections = allowed_scenarios_eq_cb) %>%
    dplyr::filter(.data$scenario %in% allowed_scenarios_eq_cb) %>%
    # TODO: temporary fix, remove once all scenario data is used from scenario file
    filter(!(scenario == "ETP2017_NPS" & ald_sector == "Power")) %>%
    dplyr::mutate(scenario = sub(".*?_", "", scenario)) %>%
    check_portfolio_consistency(start_year = start_year) %>%
    mutate(scenario = str_replace(scenario, "NPSRTS", "NPS")) %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      nesting(!!!syms(nesting_vars_lookup))
    ) %>%
    mutate(plan_tech_prod = dplyr::if_else(is.na(plan_tech_prod), 0, plan_tech_prod)) %>%
    apply_filters(
      investor = investor_name_placeholder,
      sectors = sectors_lookup,
      technologies = technologies_lookup,
      scenario_geography_filter = scenario_geography_filter,
      scenarios = scenarios_filter,
      allocation_method = allocation_method_lookup,
      start_analysis = start_year
    ) %>%
    filter(
      allocation == allocation_method_lookup,
      equity_market == equity_market_filter
    ) %>%
    distinct_all()

}
