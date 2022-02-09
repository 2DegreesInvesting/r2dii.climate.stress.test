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
                                  equity_market_filter, sectors, technologies,
                                  allocation_method, asset_type) {
  data_processed <- data %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon
    ) %>%
    dplyr::filter(.data$investor_name == investor_name_placeholder) %>%
    dplyr::filter(.data$equity_market == equity_market_filter) %>%
    dplyr::filter(.data$allocation == allocation_method) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)) %>%
    check_level_availability(
      expected_levels_list =
        list(
          year = start_year:(start_year + time_horizon),
          allocation = allocation_method,
          equity_market = equity_market_filter,
          scenario = scenarios_filter,
          scenario_geography = scenario_geography_filter
        )
    ) %>%
    report_missing_col_combinations(col_names = c("allocation", "equity_market", "scenario", "scenario_geography", "technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_pacta_results)
  # TODO: Add missingness check once pacta results input is overhauled

  if (asset_type == "bonds") {
    data_processed %>%
      dplyr::rename(corporate_bond_ticker = .data$id) %>%
      dplyr::filter(!is.na(corporate_bond_ticker)) %>%
      dplyr::select(company_name, corporate_bond_ticker) %>%
      dplyr::distinct() %>%
      check_company_ticker_mapping()
  }

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
    check_level_availability(
      expected_levels_list =
        list(
          year = start_year:end_year,
          scenario = scenarios_filter,
          scenario_geography = scenario_geography_filter,
          technology = technologies[grep("Cap", technologies)] # when checking for expected levels of technology variable only expecte power sector levels
        )
    ) %>%
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
    check_sector_tech_mapping(sector_col = "sector") %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    check_level_availability(
      expected_levels_list =
        list(
          year = start_year:end_year,
          sector = sectors,
          technology = technologies
        )
    ) %>%
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
    check_sector_tech_mapping() %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    check_level_availability(
      expected_levels_list =
        list(
          year = start_year:end_year,
          ald_sector = sectors,
          scenario = scenarios_filter,
          scenario_geography = scenario_geography_filter,
          technology = technologies
        )
    ) %>%
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


#' Process data of type indicated by function name
#'
#' NOTE returns NULL if `data` is NULL.
#'
#' @inheritParams process_pacta_results
#' @param fallback_term Numeric, holding fallback term.
#'
#' @return A tibble of data as indicated by function name.
process_company_terms <- function(data, fallback_term) {
  if (is.null(data)) {
    return(data)
  }

  data_processed <- data %>%
    check_company_terms() %>%
    dplyr::mutate(term = as.double(term)) %>%
    fill_na_terms(fallback_term) %>%
    cap_terms() %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_company_terms)

  return(data_processed)
}

st_process <- function(data, asset_type, company_exclusion, fallback_term) {
  start_year <- get_start_year(data)
  scenarios_filter <- scenarios_filter()

  sector_exposures <- process_sector_exposures(
    data$sector_exposures,
    asset_type = asset_type
  )

  capacity_factors_power <- process_capacity_factors_power(
    data$capacity_factors,
    scenarios_filter = scenarios_filter,
    scenario_geography_filter = scenario_geography_filter_lookup,
    technologies = technologies_lookup,
    start_year = start_year,
    end_year = end_year_lookup
  )

  excluded_companies <- process_excluded_companies(
    data$excluded_companies,
    company_exclusion = company_exclusion,
    technologies = technologies_lookup
  )

  df_price <- process_df_price(
    data$df_price,
    technologies = technologies_lookup,
    sectors = sectors_lookup,
    start_year = start_year,
    end_year = end_year_lookup
  )

  scenario_data <- process_scenario_data(
    data$scenario_data,
    start_year = start_year,
    end_year = end_year_lookup,
    sectors = sectors_lookup,
    technologies = technologies_lookup,
    scenario_geography_filter = scenario_geography_filter_lookup,
    scenarios_filter = scenarios_filter
  )

  financial_data <- process_financial_data(
    data$financial_data,
    asset_type = asset_type
  )

  company_terms <- process_company_terms(
    data$company_terms,
    fallback_term = fallback_term
  )

  pacta_results <- process_pacta_results(
    data$pacta_results,
    start_year = start_year,
    end_year = end_year_lookup,
    time_horizon = time_horizon_lookup,
    scenario_geography_filter = scenario_geography_filter_lookup,
    scenarios_filter = scenarios_filter,
    equity_market_filter = equity_market_filter_lookup,
    sectors = sectors_lookup,
    technologies = technologies_lookup,
    allocation_method = allocation_method_lookup,
    asset_type = asset_type
  ) %>%
    add_terms(company_terms = company_terms, fallback_term = fallback_term)

  out <- list(
    pacta_results = pacta_results,
    sector_exposures = sector_exposures,
    capacity_factors_power = capacity_factors_power,
    excluded_companies = excluded_companies,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data,
    company_terms = company_terms
  )

  return(out)
}
