#' Process data of type indicated by function name
#'
#' @inheritParams run_stress_test
#' @inheritParams report_company_drops
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
                                  allocation_method, asset_type, log_path) {
  data_processed <- data %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon
    ) %>%
    set_initial_plan_carsten_missings_to_zero(
      start_year = start_year,
      time_horizon = time_horizon
    ) %>%
    is_scenario_geography_in_pacta_results(scenario_geography_filter) %>%
    dplyr::filter(.data$investor_name == investor_name_placeholder) %>%
    dplyr::filter(.data$equity_market == equity_market_filter) %>%
    dplyr::filter(.data$allocation == allocation_method) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)) %>%
    remove_companies_with_missing_exposures(
      start_year = start_year,
      time_horizon = time_horizon,
      log_path = log_path
    ) %>%
    remove_sectors_with_missing_production_end_of_forecast(
      start_year = start_year,
      time_horizon = time_horizon,
      log_path = log_path
    ) %>%
    remove_sectors_with_missing_production_start_year(
      start_year = start_year,
      log_path = log_path
    ) %>%
    remove_high_carbon_tech_with_missing_production(
      start_year = start_year,
      time_horizon = time_horizon,
      log_path = log_path
    ) %>%
    stop_if_empty(data_name = "Pacta Results") %>%
    check_level_availability(
      data_name = "Pacta Results",
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

  if (asset_type == "bonds") {
    data_processed %>%
      dplyr::rename(corporate_bond_ticker = .data$id) %>%
      dplyr::filter(!is.na(corporate_bond_ticker)) %>%
      dplyr::select(company_name, corporate_bond_ticker) %>%
      dplyr::distinct() %>%
      check_company_ticker_mapping()
  }

  # since pacta for loans returns only NA values for id, we ignore the
  # column when checking for missing values
  if (asset_type == "loans") {
    data_processed %>%
      dplyr::select(-.data$id) %>%
      report_missings(name_data = "pacta data", throw_error = TRUE)
  } else {
    data_processed %>%
      report_missings(name_data = "pacta data", throw_error = TRUE)
  }

  return(data_processed)
}

is_scenario_geography_in_pacta_results <- function(data, scenario_geography_filter) {
  if (!scenario_geography_filter %in% unique(data$scenario_geography)) {
    stop(paste0(
      "Did not find PACTA results for scenario_geography level ", scenario_geography_filter,
      ". Please check PACTA results or pick another scenario_geography."
    ))
  }
  invisible(data)
}

#' Remove rows from PACTA results that belong to company-technology combinations
#' for which there is no information on the exposure in the portfolio. We join
#' company results on the portfolio exposure based on the last available year of
#' the production forecast. Hence we filter for missings in that year.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
remove_companies_with_missing_exposures <- function(data,
                                                    start_year,
                                                    time_horizon,
                                                    log_path) {
  n_companies_pre <- length(unique(data$company_name))

  # we merge the exposure of the last year of the forecast on the company
  # results for the aggregation, to get the closest picture to the shock year.
  # Hence start_year + time_horizon
  companies_missing_exposure_value <- data %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::filter(is.na(.data$plan_carsten))

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_exposure_value,
      by = c("company_name", "technology")
    )

  n_companies_post <- length(unique(data_filtered$company_name))

  if (n_companies_pre > n_companies_post) {
    percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
    affected_companies <- sort(
      setdiff(
        data$company_name,
        data_filtered$company_name
      )
    )
    paste_write(
      format_indent_1(), "When filtering out holdings with exposures missing value, dropped rows for",
      n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
  }


  return(data_filtered)
}

#' Rows that have no information on the exposure before the end of the
#' production forecast, should get a zero exposure value. This allows keeping
#' the information for cases where production is only being built out toward the
#' end of the forecast period.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
set_initial_plan_carsten_missings_to_zero <- function(data,
                                                      start_year,
                                                      time_horizon) {
  data <- data %>%
    dplyr::mutate(
      plan_carsten = dplyr::if_else(
        .data$year < .env$start_year + .env$time_horizon &
          is.na(.data$plan_carsten),
        0,
        .data$plan_carsten
      )
    ) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario, .data$scenario_geography, .data$ald_sector,
      .data$technology, .data$year
    )

  return(data)
}

#' Remove rows from PACTA results that belong to company-sector combinations
#' for which there is no positive production value in the relevant year of
#' exposure (last year of forecast). This handles the edge case that a company
#' may have a positive exposure for this sector, but none of the technologies
#' covered in this analysis have any positive production. Such inconsistencies
#' may arise e.g. because of unclear separation of the LDV and HDV sectors.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
remove_sectors_with_missing_production_end_of_forecast <- function(data,
                                                                   start_year,
                                                                   time_horizon,
                                                                   log_path) {
  n_companies_pre <- length(unique(data$company_name))

  companies_missing_sector_production <- data %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::group_by(
      .data$company_name, .data$scenario, .data$ald_sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  # while this technically removes problematic cases for only certain scenarios
  # for a company, this will in practice not lead to one scenario being removed
  # and another remaining in the data because the production plans are the same
  # across scenarios.
  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production,
      by = c("company_name", "scenario", "ald_sector")
    )

  n_companies_post <- length(unique(data_filtered$company_name))

  if (n_companies_pre > n_companies_post) {
    percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
    affected_companies <- sort(
      setdiff(
        data$company_name,
        data_filtered$company_name
      )
    )
    paste_write(
      format_indent_1(), "When filtering out holdings with 0 production in relevant sector, dropped rows for",
      n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
  }


  return(data_filtered)
}

#' Remove rows from PACTA results that belong to company-sector combinations
#' for which there is no positive production value in the relevant start year.
#' This handles the edge case that a company may have a green technology with
#' zero initial production that should grow over time, but since the overall
#' sector production is also zero in the start year, the SMSP is unable to
#' calculate positive targets.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
remove_sectors_with_missing_production_start_year <- function(data,
                                                              start_year,
                                                              log_path) {
  n_companies_pre <- length(unique(data$company_name))

  companies_missing_sector_production_start_year <- data %>%
    dplyr::filter(.data$year == .env$start_year) %>%
    dplyr::group_by(
      .data$company_name, .data$scenario, .data$ald_sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  # while this technically removes problematic cases for only certain scenarios
  # for a company, this will in practice not lead to one scenario being removed
  # and another remaining in the data because the production plans are the same
  # across scenarios.
  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production_start_year,
      by = c("company_name", "scenario", "ald_sector")
    )

  n_companies_post <- length(unique(data_filtered$company_name))

  if (n_companies_pre > n_companies_post) {
    percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
    affected_companies <- sort(
      setdiff(
        data$company_name,
        data_filtered$company_name
      )
    )
    paste_write(
      format_indent_1(), "When filtering out holdings with 0 production in
      relevant sector in the start year of the analysis, dropped rows for",
      n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
  }


  return(data_filtered)
}

#' Remove rows from PACTA results that belong to company-technology combinations
#' for which there is 0 production in a high carbon technology over the entire
#' forecast. Since this technology would need to decrease in its targets, the
#' production remains zero and creates missing values later on. The combination
#' is therefore removed.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#' @param data tibble containing filtered PACTA results
#'
#' @return A tibble of data without rows with no exposure info
#' @noRd
remove_high_carbon_tech_with_missing_production <- function(data,
                                                            start_year,
                                                            time_horizon,
                                                            log_path) {
  n_companies_pre <- length(unique(data$company_name))

  companies_missing_high_carbon_tech_production <- data %>%
    dplyr::filter(.data$technology %in% c("ICE", "Coal", "Oil", "Gas", "CoalCap", "GasCap", "OilCap")) %>%
    dplyr::group_by(
      .data$company_name, .data$scenario, .data$technology
    ) %>%
    dplyr::summarise(
      technology_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$technology_prod <= 0)

  # while this technically removes problematic cases for only certain scenarios
  # for a company, this will in practice not lead to one scenario being removed
  # and another remaining in the data because the production plans are the same
  # across scenarios.
  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_high_carbon_tech_production,
      by = c("company_name", "scenario", "technology")
    )

  n_companies_post <- length(unique(data_filtered$company_name))

  if (n_companies_pre > n_companies_post) {
    percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
    affected_companies <- sort(
      setdiff(
        data$company_name,
        data_filtered$company_name
      )
    )
    paste_write(
      format_indent_1(), "When filtering out holdings with 0 production in given high-carbon technology, dropped rows for",
      n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
  }


  return(data_filtered)
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
    stop_if_empty(data_name = "Sector Exposures") %>%
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
    harmonise_cap_fac_geo_names() %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Capacity Factors") %>%
    check_level_availability(
      data_name = "Capacity Factors",
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

harmonise_cap_fac_geo_names <- function(data) {
  data <- data %>%
    # hardcoded adjustments are needed here for compatibility with P4I
    dplyr::mutate(scenario_geography = gsub(" ", "", scenario_geography, fixed = TRUE)) %>%
    dplyr::mutate(scenario_geography = dplyr::case_when(
      scenario_geography == "EuropeanUnion" ~ "EU",
      scenario_geography == "Non-OECD" ~ "NonOECD",
      scenario_geography == "UnitedStates" ~ "US",
      TRUE ~ scenario_geography
    ))
  return(data)
}


#' Process data of type indicated by function name
#'
#' @inheritParams process_pacta_results
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_price_data <- function(data, technologies, sectors, start_year, end_year,
                               scenarios_filter) {
  data_processed <- data %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    check_sector_tech_mapping(sector_col = "ald_sector") %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Price Data") %>%
    check_level_availability(
      data_name = "Price Data",
      expected_levels_list =
        list(
          year = start_year:end_year,
          ald_sector = sectors,
          technology = technologies,
          scenario = scenarios_filter
        )
    ) %>%
    report_missing_col_combinations(col_names = c("scenario", "technology", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_price_data) %>%
    report_missings(name_data = "price data", throw_error = TRUE) %>%
    tidyr::pivot_wider(names_from = "scenario", values_from = "price", names_prefix = "price_")

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
    stop_if_empty(data_name = "Scenario Data") %>%
    check_sector_tech_mapping() %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Scenario Data") %>%
    check_level_availability(
      data_name = "Scenario Data",
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
    stop_if_empty(data_name = "Financial Data") %>%
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

st_process <- function(data, asset_type, fallback_term,
                       scenario_geography, baseline_scenario, shock_scenario,
                       sectors, technologies,
                       log_path) {
  start_year <- get_start_year(data)
  scenarios_filter <- c(baseline_scenario, shock_scenario)

  sector_exposures <- process_sector_exposures(
    data$sector_exposures,
    asset_type = asset_type
  )

  capacity_factors_power <- process_capacity_factors_power(
    data$capacity_factors_power,
    scenarios_filter = scenarios_filter,
    scenario_geography_filter = scenario_geography,
    technologies = technologies,
    start_year = start_year,
    end_year = end_year_lookup
  )

  df_price <- process_price_data(
    data$df_price,
    technologies = technologies,
    sectors = sectors,
    start_year = start_year,
    end_year = end_year_lookup,
    scenarios_filter = scenarios_filter
  )

  scenario_data <- process_scenario_data(
    data$scenario_data,
    start_year = start_year,
    end_year = end_year_lookup,
    sectors = sectors,
    technologies = technologies,
    scenario_geography_filter = scenario_geography,
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
    scenario_geography_filter = scenario_geography,
    scenarios_filter = scenarios_filter,
    equity_market_filter = equity_market_filter_lookup,
    sectors = sectors,
    technologies = technologies,
    allocation_method = allocation_method_lookup,
    asset_type = asset_type,
    log_path = log_path
  ) %>%
    add_terms(company_terms = company_terms, fallback_term = fallback_term)

  out <- list(
    pacta_results = pacta_results,
    sector_exposures = sector_exposures,
    capacity_factors_power = capacity_factors_power,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data,
    company_terms = company_terms
  )

  return(out)
}
