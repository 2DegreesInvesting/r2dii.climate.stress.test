#' Process data of type indicated by function name
#'
#' @inheritParams run_stress_test
#' @param data A tibble of data of type indicated by function name.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Numeric, holding time horizon of analysis.
#' @param scenario_geography_filter Character. A vector of length 1 that
#'   indicates which geographic scenario to apply in the analysis.
#' @param scenarios_filter Vector holding baseline and shock scenario name.
#' @param equity_market_filter Character. A vector of length 1 that
#'   indicates which equity market to apply in the analysis.
#' @param sectors Character vector, holding considered sectors.
#' @param technologies Character vector, holding considered technologies.
#' @param allocation_method Character. A vector of length 1 indicating the
#'   set of PACTA data to be used in the analysis, based on the choice of an
#'   allocation rule.
#'
#' @return A tibble of data as indicated by function name.
process_pacta_results <- function(data, start_year, end_year, time_horizon,
                                  scenario_geography_filter, scenarios_filter,
                                  equity_market_filter, term, sectors, technologies,
                                  allocation_method) {
  data_processed <- data %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon,
      scenario_geography_filter = scenario_geography_filter,
      scenarios_filter = scenarios_filter,
      allocation_method = allocation_method
    ) %>%
    dplyr::filter(.data$equity_market == equity_market_filter) %>%
    dplyr::filter(.data$allocation == allocation_method) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    # ADO 1943 - for the time being, one global term value is set by the user.
    # TODO: ADO 3182 - allow setting loan level term
    dplyr::mutate(term = term) %>%
    report_missing_col_combinations(col_names = c("allocation", "equity_market", "scenario", "scenario_geography", "technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_pacta_results)
  # TODO: Add missingness check once pacta results input is overhauled

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#' @inheritParams run_stress_test
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_sector_exposures <- function(data, asset_type) {
  data_processed <- data %>%
    wrangle_and_check_sector_exposures(asset_type = asset_type) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_sector_exposures) %>%
    report_missings(name_data = "sector exposures", throw_error = TRUE)

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
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
    report_missing_col_combinations(col_names = c("scenario", "scenario_geography", "technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_capacity_factors_power) %>%
    report_missings(name_data = "capacity factors", throw_error = TRUE)

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#' @inheritParams run_stress_test
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_excluded_companies <- function(data, company_exclusion, technologies) {
  if (!company_exclusion) {
    return(NULL)
  }

  data_processed <- data %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_company_exclusion) %>%
    report_missings(name_data = "company exclusion", throw_error = TRUE)

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_df_price <- function(data, technologies, sectors, start_year, end_year) {
  data_processed <- data %>%
    dplyr::filter(.data$sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_missing_col_combinations(col_names = c("technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_price_data)
  # TODO: add reporting on missing after switching to long price data

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_scenario_data <- function(data, start_year, end_year, sectors, technologies,
                                  scenario_geography_filter, scenarios_filter) {
  data_processed <- data %>%
    wrangle_scenario_data(start_year = start_year, end_year = end_year) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_missing_col_combinations(col_names = c("scenario", "scenario_geography", "technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_scenario_data) %>%
    report_missings(name_data = "scenario data", throw_error = TRUE)

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#' @inheritParams run_stress_test
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_financial_data <- function(data, asset_type) {
  data_processed <- data %>%
    check_financial_data(asset_type = asset_type) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_financial_data) %>%
    report_missings(name_data = "financial data", throw_error = TRUE)

  return(data_processed)
}
