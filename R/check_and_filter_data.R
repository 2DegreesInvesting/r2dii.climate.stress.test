check_and_filter_data <- function(st_data_list, start_year, end_year, scenarios_filter, scenario_geography_filter) {

  capacity_factors_power_filtered <- st_data_list$capacity_factors_power %>%
    dplyr::filter(.data$scenario %in% scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, start_year, end_year))

  excluded_companies_filtered <- st_data_list$excluded_companies %>%
    dplyr::filter(.data$technology %in% technologies_lookup)

  df_price_filtered <- st_data_list$df_price %>%
    dplyr::filter(.data$sector %in% sectors_lookup) %>%
    dplyr::filter(.data$technology %in% technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, start_year, end_year))

  scenario_data_filtered <- st_data_list$scenario_data %>%
    dplyr::filter(.data$scenario %in% scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% sectors_lookup) %>%
    dplyr::filter(.data$technology %in% technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, start_year, end_year))

  financial_data_filtered <- st_data_list$financial_data %>%
    dplyr::filter(.data$ald_sector %in% sectors_lookup) %>%
    dplyr::filter(.data$technology %in% technologies_lookup)

  pacta_results_filtered <- st_data_list$pacta_results %>%
    dplyr::filter(.data$scenario %in% scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% sectors_lookup) %>%
    dplyr::filter(.data$technology %in% technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, start_year, end_year))

  sector_exposures_filtered <- st_data_list$sector_exposures %>%
    dplyr::filter(.data$financial_sector %in% sectors_lookup)

  report_all_duplicate_kinds(
    data = capacity_factors_power_filtered,
    composite_unique_cols = c("scenario", "scenario_geography", "technology", "year"))

  report_all_duplicate_kinds(
    data = excluded_companies_filtered,
    composite_unique_cols = c("company_name", "technology"))

  report_all_duplicate_kinds(
    data = df_price_filtered,
    composite_unique_cols = c("year", "sector", "technology"))

  report_all_duplicate_kinds(
    data = scenario_data_filtered,
    composite_unique_cols = c("scenario_geography", "scenario", "ald_sector", "technology", "year"))

  report_all_duplicate_kinds(
    data = financial_data_filtered,
    composite_unique_cols = c("company_name", "company_id", "ald_sector", "technology"))

  report_all_duplicate_kinds(
    data = pacta_results_filtered,
    composite_unique_cols = c("year", "equity_market", "ald_sector", "technology", "scenario", "allocation", "scenario_geography", "company_name", "id"))

  report_all_duplicate_kinds(
    data = sector_exposures_filtered,
    composite_unique_cols = c("financial_sector"))

  return(list(
    capacity_factors_power = capacity_factors_power_filtered,
    excluded_companies = excluded_companies_filtered,
    df_price = df_price_filtered,
    scenario_data = scenario_data_filtered,
    financial_data = financial_data_filtered,
    pacta_results = pacta_results_filtered,
    sector_exposures = sector_exposures_filtered
  ))
}
