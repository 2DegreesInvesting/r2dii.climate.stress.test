process_pacta_results <- function(data, start_year, end_year, time_horizon, scenario_geography_filter, scenarios_filter, equity_market_filter, term, sectors, technologies) {

  data_processed <- data %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon,
      scenario_geography_filter = scenario_geography_filter,
      scenarios_filter = scenarios_filter,
      equity_market_filter = equity_market_filter
    ) %>%
    # ADO 1943 - for the time being, one global term value is set by the user.
    # TODO: ADO 3182 - allow setting loan level term
    dplyr::mutate(term = term) %>%
      dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
      dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
      dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
      dplyr::filter(.data$technology %in% .env$technologies) %>%
      dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_pacta_results)
  # TODO: Add missingness check once pacta results input is overhauled

  return(data_processed)

}

process_sector_exposures <- function(data, asset_type) {
  data_processed <- data %>%
    wrangle_and_check_sector_exposures(asset_type = asset_type) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_sector_exposures) %>%
    report_missings(name_data = "sector exposures", throw_error = TRUE)

  return(data_processed)
}

process_capacity_factors_power <- function(data,
                                           scenarios_filter,
                                           scenario_geography_filter,
                                           technologies,
                                           start_year,
                                           end_year) {
  data_processed <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_capacity_factors_power) %>%
    report_missings(name_data = "capacity factors", throw_error = TRUE)

  return(data_processed)
}

process_excluded_companies <- function(data, technologies) {
  data_processed <- data %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_company_exclusion) %>%
    report_missings(name_data = "company exclusion", throw_error = TRUE)

  return(data_processed)
}

process_df_price <- function(data, technologies, sectors, start_year, end_year) {
  data_processed <- data %>%
    check_technology_availability(expected_technologies = technologies) %>%
    dplyr::filter(year >= start_year) %>%
    check_price_consistency(start_year = start_year) %>%
    dplyr::filter(.data$sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_price_data)
    # TODO: add reporting on missing after switching to long pricendata

  return(data_processed)
}

process_scenario_data <- function(data, start_year, end_year, sectors, technologies, scenario_geography_filter, scenarios_filter) {
  data_processed <- data %>%
    wrangle_scenario_data(start_year = start_year, end_year = end_year) %>%
    dplyr::filter(
      .data$ald_sector %in% sectors &
        .data$technology %in% technologies &
        .data$scenario_geography == scenario_geography_filter
    ) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_scenario_data) %>%
    report_missings(name_data = "scenario data", throw_error = TRUE)

  return(data_processed)
}

process_financial_data <- function(data, asset_type) {
  data_processed <- data %>%
    check_financial_data(asset_type = asset_type) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_financial_data) %>%
    report_missings(name_data = "financial data", throw_error = TRUE)

  return(data_processed)
}
