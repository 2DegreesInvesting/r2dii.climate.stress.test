is_scenario_geography_in_pacta_results <- function(data, scenario_geography_filter) {
  if (!scenario_geography_filter %in% unique(data$scenario_geography)) {
    stop(paste0(
      "Did not find PACTA results for scenario_geography level ", scenario_geography_filter,
      ". Please check PACTA results or pick another scenario_geography."
    ))
  }
  invisible(data)
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
      .data$company_name, .data$ald_sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production,
      by = c("company_name", "ald_sector")
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
#' This handles the edge case that a company may have a green ald_business_unit with
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
      .data$company_name, .data$ald_sector
    ) %>%
    dplyr::summarise(
      sector_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$sector_prod <= 0)

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_sector_production_start_year,
      by = c("company_name", "ald_sector")
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

#' Remove rows from PACTA results that belong to company-ald_business_unit combinations
#' for which there is 0 production in a high carbon ald_business_unit over the entire
#' forecast. Since this ald_business_unit would need to decrease in its targets, the
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
  companies_missing_high_carbon_tech_production <- data %>%
    dplyr::filter(.data$ald_business_unit %in% high_carbon_tech_lookup) %>%
    dplyr::group_by(
      .data$company_name, .data$ald_sector, .data$ald_business_unit
    ) %>%
    dplyr::summarise(
      technology_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$technology_prod <= 0)

  data_filtered <- data %>%
    dplyr::anti_join(
      companies_missing_high_carbon_tech_production,
      by = c("company_name", "ald_sector", "ald_business_unit")
    )

  if (nrow(companies_missing_high_carbon_tech_production) > 0) {
    # information on companies for which at least 1 ald_business_unit is lost
    affected_company_sector_tech_overview <- companies_missing_high_carbon_tech_production %>%
      dplyr::select(dplyr::all_of(c("company_name", "ald_sector", "ald_business_unit"))) %>%
      dplyr::distinct_all()

    percent_affected_companies <- (length(unique(affected_company_sector_tech_overview$company_name)) * 100) / length(unique(data$company_name))
    affected_companies <- affected_company_sector_tech_overview$company_name

    paste_write(
      format_indent_1(), "When filtering out holdings with 0 production in given high-carbon ald_business_unit, dropped rows for",
      length(affected_companies), "out of", length(unique(data$company_name)), "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_affected_companies, log_path = log_path)
    paste_write(format_indent_2(), "affected company-sector-ald_business_unit combinations:", log_path = log_path)

    affected_company_sector_tech_overview %>%
      purrr::pwalk(function(company_name, ald_sector, ald_business_unit) {
        paste_write(format_indent_2(), "company name:", company_name, "sector:", ald_sector, "ald_business_unit:", ald_business_unit, log_path = log_path)
      })
  }

  return(data_filtered)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
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
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Capacity Factors") %>%
    check_level_availability(
      data_name = "Capacity Factors",
      expected_levels_list =
        list(
          year = start_year:end_year,
          scenario = scenarios_filter,
          scenario_geography = scenario_geography_filter,
          ald_business_unit = technologies[grep("Cap", technologies)] # when checking for expected levels of ald_business_unit variable only expecte power sector levels
        )
    ) %>%
    report_missing_col_combinations(col_names = c("scenario", "scenario_geography", "ald_business_unit", "year")) %>%
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
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_price_data <- function(data, technologies, sectors, start_year, end_year,
                               scenarios_filter) {

  data_processed <- data %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    check_sector_tech_mapping(sector_col = "ald_sector") %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    stop_if_empty(data_name = "Price Data") %>%
    check_level_availability(
      data_name = "Price Data",
      expected_levels_list =
        list(
          year = start_year:end_year,
          ald_sector = sectors,
          ald_business_unit = technologies,
          scenario = scenarios_filter
        )
    ) %>%
    report_missing_col_combinations(col_names = c("scenario", "ald_business_unit", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_price_data) %>%
    report_missings(name_data = "price data", throw_error = TRUE) %>%
    tidyr::pivot_wider(names_from = "scenario", values_from = "price", names_prefix = "price_")

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_scenario_data <- function(data, start_year, end_year, sectors, technologies,
                                  scenario_geography_filter, scenarios_filter) {

  data_processed <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    stop_if_empty(data_name = "Scenario Data") %>%
    check_sector_tech_mapping() %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
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
          ald_business_unit = technologies
        )
    ) %>%
    report_missing_col_combinations(col_names = c("scenario", "scenario_geography", "ald_business_unit", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_scenario_data) %>%
    report_missings(name_data = "scenario data", throw_error = TRUE)

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_carbon_data <- function(data, start_year, end_year, carbon_price_model) {
  data_processed <- data

  ## dataframe will be NULL for lrisk this is the case as lrisk does not read in and use carbon prices
  if (is.null(data_processed)) {
    data_processed <- NULL
  } else {
    data_processed <- data_processed %>%
      dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
      dplyr::select(-c(scenario_geography)) %>%
      dplyr::filter(.data$scenario %in% .env$carbon_price_model) %>%
      stop_if_empty(data_name = "Carbon Data")
  }

  return(data_processed)
}

#' Process data of type indicated by function name
#'
#' @inheritParams process_production_data
#' @inheritParams run_trisk
#'
#' @return A tibble of data as indicated by function name.
#' @noRd
process_financial_data <- function(data) {
  data_processed <- data %>%
    stop_if_empty(data_name = "Financial Data") %>%
    check_financial_data() %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_financial_data) %>%
    report_missings(name_data = "financial data", throw_error = TRUE)

  return(data_processed)
}

st_process <- function(data, scenario_geography, baseline_scenario,
                       shock_scenario, start_year, carbon_price_model,
                       log_path, end_year) {

  scenarios_filter <- c(baseline_scenario, shock_scenario)

  sectors_and_technologies_list <- infer_sectors_and_technologies(
    price_data=data$df_price,
    scenario_data=data$scenario_data,
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography
  )

  df_price <- process_price_data(
    data$df_price,
    technologies = sectors_and_technologies_list$technologies,
    sectors = sectors_and_technologies_list$sectors,
    start_year = start_year,
    end_year = end_year,
    scenarios_filter = scenarios_filter
  )

  scenario_data <- process_scenario_data(
    data$scenario_data,
    start_year = start_year,
    end_year = end_year,
    sectors = sectors_and_technologies_list$sectors,
    technologies = sectors_and_technologies_list$technologies,
    scenario_geography_filter = scenario_geography,
    scenarios_filter = scenarios_filter
  )

  financial_data <- process_financial_data(
    data$financial_data
  )


  carbon_data <- process_carbon_data(
    data$carbon_data,
    start_year = start_year,
    end_year = end_year,
    carbon_price_model = carbon_price_model
  )

  production_data <- process_production_data(
    data$production_data,
    start_year = start_year,
    end_year = end_year,
    time_horizon = time_horizon_lookup,
    scenario_geography_filter = scenario_geography,
    sectors = sectors_and_technologies_list$sectors,
    technologies = sectors_and_technologies_list$technologies,
    log_path = log_path
  )

  # add extend production data with scenario targets
  production_data <- production_data %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
      start_analysis = start_year,
      end_analysis = end_year,
      time_frame = time_horizon_lookup,
      target_scenario = shock_scenario
    )

  # capacity_factors are only applied for power sector
  if ("Power" %in% sectors_and_technologies_list$sectors) {
    capacity_factors_power <- process_capacity_factors_power(
      data$capacity_factors_power,
      scenarios_filter = scenarios_filter,
      scenario_geography_filter = scenario_geography,
      technologies = sectors_and_technologies_list$technologies,
      start_year = start_year,
      end_year = end_year
    )

    # convert power capacity to generation
    production_data <- convert_power_cap_to_generation(
      data = production_data,
      capacity_factors_power = capacity_factors_power,
      baseline_scenario = baseline_scenario,
      target_scenario = shock_scenario
    )
  } else {
    capacity_factors_power <- data$capacity_factors_power
  }

  out <- list(
    capacity_factors_power = capacity_factors_power,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data,
    production_data = production_data,
    carbon_data = carbon_data
  )

  return(out)
}

#' Process data of type indicated by function name
#'
#' @inheritParams run_trisk
#' @inheritParams report_company_drops
#' @param data A tibble of data of type indicated by function name.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Numeric, holding time horizon of production data.
#' @param scenario_geography_filter Character. A vector of length 1 that
#'   indicates which geographic scenario to apply in the analysis.
#' @param sectors Character vector, holding considered sectors.
#' @param technologies Character vector, holding considered technologies.
#'
#' @return A tibble of data as indicated by function name.
process_production_data <- function(data, start_year, end_year, time_horizon,
                                    scenario_geography_filter, sectors,
                                    technologies, log_path) {
  data_processed <- data %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors) %>%
    dplyr::filter(.data$ald_business_unit %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)) %>%
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
    stop_if_empty(data_name = "Production Data") %>%
    check_level_availability(
      data_name = "Production Data",
      expected_levels_list =
        list(
          year = start_year:(start_year + time_horizon),
          scenario_geography = scenario_geography_filter,
          ald_sector = sectors,
          ald_business_unit = technologies
        ),
      throw_error = FALSE
    ) %>%
    report_missing_col_combinations(col_names = c("scenario_geography", "ald_business_unit", "year")) %>%
    report_all_duplicate_kinds(composite_unique_cols = cuc_production_data)

  # checks that no missing values exist in the data
  data_processed %>%
    report_missings(name_data = "production data", throw_error = TRUE)

  return(data_processed)
}
