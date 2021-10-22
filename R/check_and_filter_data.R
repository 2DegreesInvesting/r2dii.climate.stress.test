check_and_filter_data <- function(st_data_list, start_year, end_year, scenarios_filter, scenario_geography_filter) {

  capacity_factors_power_filtered <- st_data_list$capacity_factors_power %>%
    dplyr::filter(.data$scenario %in% scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, start_year, end_year))

  report_all_duplicate_kinds(
    data = capacity_factors_power_filtered,
    composite_unique_cols = c("scenario", "scenario_geography", "technology", "year"))

  return(list(
    capacity_factors_power = capacity_factors_power_filtered,
    excluded_companies = excluded_companies_filtered,
    df_price = df_price_filtered,
    scenario_data = scenario_data_filtered,
    financial_data = financial_data_filtered
  ))
}
