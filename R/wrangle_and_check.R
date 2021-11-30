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
wrangle_and_check_sector_exposures <- function(sector_exposures, asset_type) {
  asset_type <- stringr::str_to_title(asset_type)

  if (!asset_type %in% c("Bonds", "Equity", "Loans")) {
    stop("Invalid asset type", call. = FALSE)
  }

  if (!is.logical(sector_exposures$valid_input)) {
    stop("Column valid_input needs to be of type logical.", call. = FALSE)
  }

  if (!is.character(sector_exposures$financial_sector)) {
    stop("Column financial sector needs to be of type character.", call. = FALSE)
  }

  if (any(sector_exposures$valid_value_usd < 0)) {
    affected_sectors <- sector_exposures %>%
      dplyr::filter(valid_value_usd < 0) %>%
      dplyr::pull(.data$financial_sector)

    stop(paste0("Asset under management has negative value in sector(s) ", paste0(affected_sectors, collapse = ", "), ".
                This is not supported by the analysis."), call. = FALSE)
  }

  report_missings(
    data = sector_exposures,
    name_data = "sector exposures"
  )

  report_all_duplicate_kinds(
    data = sector_exposures,
    composite_unique_cols = c(
      "investor_name", "portfolio_name", "asset_type", "financial_sector", "valid_input"
    )
  )

  valid_sector_exposures <- sector_exposures %>%
    dplyr::filter(valid_input) %>%
    dplyr::filter(asset_type == !!asset_type) %>%
    dplyr::select(-valid_input, -asset_type)

  if (nrow(valid_sector_exposures) == 0) {
    stop("No valid sector exposures available.")
  }

  return(valid_sector_exposures)
}


#' Wrangle and check PACTA results
#'
#' Function applies several filter steps to
#' 1. restrict PACTA results to scenarios useable for stresstesting
#' 1. to apply run/project related selections.
#'
#' Also consistency checks are run.
#'
#' @param pacta_results Results from PACTA analysis.
#' @param start_year First considered year in analysis.
#' @param time_horizon Considered timefram for PACTA analysis.
#' @param scenario_geography_filter Character. A vector of length 1 that
#'   indicates which geographic scenario to apply in the analysis.
#' @param scenarios_filter Vector holding baseline and shock scenario name.
#' @param equity_market_filter Character. A vector of length 1 that
#'   indicates which equity market to apply in the analysis.
#'
#' @return Wrangled `pacta_results.`
wrangle_and_check_pacta_results <- function(pacta_results, start_year, time_horizon,
                                            scenario_geography_filter, scenarios_filter,
                                            equity_market_filter) {
  wrangled_pacta_results <- pacta_results %>%
    select_sector_scenario_combinations() %>%
    dplyr::mutate(scenario = sub(".*?_", "", scenario)) %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      tidyr::nesting(!!!rlang::syms(nesting_vars_lookup))
    ) %>%
    dplyr::mutate(plan_tech_prod = dplyr::if_else(is.na(.data$plan_tech_prod), 0, .data$plan_tech_prod)) %>%
    apply_filters(
      investor = investor_name_placeholder,
      sectors = sectors_lookup,
      technologies = technologies_lookup,
      scenario_geography_filter = scenario_geography_filter,
      scenarios = scenarios_filter,
      allocation_method = allocation_method_lookup,
      start_analysis = start_year
    ) %>%
    dplyr::filter(
      .data$allocation == allocation_method_lookup,
      .data$equity_market == equity_market_filter
    )
}

#' Check financial data
#'
#' Applies sanity checks to financial data. Also remove column
#' corporate_bond_ticker if `asset_type` is not bonds.
#'
#' @param financial_data A data set of `financial_data`.
#' @param asset_type A string indicating if company data are for analysis for
#'   bond or equity.
#'
#' @return A prewrangled `financial_data` set.
#' @export
#' @examples
#' fin_data <- tibble::tibble(
#'   company_name = c("Firm A", "Firm B"),
#'   company_id = c(1, 2),
#'   corporate_bond_ticker = c(NA, "TICK1"),
#'   pd = c(0.01, 0.002),
#'   net_profit_margin = c(0.423, 0.2),
#'   debt_equity_ratio = c(0.1, 0.201),
#'   volatility = c(0.130, 0.299)
#' )
#'
#' check_financial_data(
#'   financial_data = fin_data,
#'   asset_type = "equity"
#' )
check_financial_data <- function(financial_data, asset_type) {
  if (!asset_type %in% c("bonds", "equity", "loans")) {
    stop("Invalid asset type.")
  }

  expected_columns <- c(
    "company_name", "company_id", "pd", "net_profit_margin",
    "debt_equity_ratio", "volatility"
  )

  if (asset_type == "bonds") {
    expected_columns <- c(expected_columns, "corporate_bond_ticker")
  }

  validate_data_has_expected_cols(
    data = financial_data,
    expected_columns = expected_columns
  )

  if (asset_type != "bonds") {
    # ADO 2493 - if asset_type not bond, ticker not required. Use distinct_all
    # to remove duplicates from remaining CUC columns since financial data is
    # always equal for these columns
    financial_data <- financial_data %>%
      dplyr::select(
        .data$company_name, .data$company_id, .data$pd, .data$net_profit_margin,
        .data$debt_equity_ratio, .data$volatility
      ) %>%
      dplyr::distinct_all()
  } else {
    financial_data <- financial_data %>% dplyr::filter(!is.na(.data$corporate_bond_ticker))
  }

  report_missings(
    data = financial_data,
    name_data = "Financial Data"
  )

  report_all_duplicate_kinds(
    data = financial_data,
    composite_unique_cols = c(
      "company_name", "company_id"
    )
  )

  check_valid_financial_data_values(
    financial_data = financial_data,
    asset_type = asset_type
  )

  return(financial_data)
}

#' Wrangle scenario data
#'
#' Function applies custom wrangling to scenario data.
#'
#' @param scenario_data A tibble holding scenario data.
#' @param start_year Start year of analysis.
#' @param end_year End year of analysis.
#'
#' @return A tibble holding wrangled scenario_data.
wrangle_scenario_data <- function(scenario_data, start_year, end_year) {
  scenario_data_wrangled <- scenario_data %>%
    dplyr::rename(source = .data$scenario_source) %>%
    dplyr::filter(.data$source %in% c("ETP2017", "WEO2019")) %>%
    # TODO: this should be set elsewhere
    dplyr::filter(!(.data$source == "ETP2017" & .data$ald_sector == "Power")) %>%
    dplyr::mutate(scenario = ifelse(stringr::str_detect(.data$scenario, "_"), stringr::str_extract(.data$scenario, "[^_]*$"), .data$scenario)) %>%
    check_scenario_timeframe(start_year = start_year, end_year = end_year) %>%
    correct_automotive_scendata(interpolation_years = c(2031:2034, 2036:2039))
  return(scenario_data_wrangled)
}


#' Fill missing values on annual_profits
#'
#' Function fill missing rows on cols company_id, pd, net_profit_margin,
#' debt_equity_ratio, volatility.
#'
#' @param annual_profits A tibble holding annual profit data.
#'
#' @return Tibble holding `annual profits` with replaces missings.
fill_annual_profit_cols <- function(annual_profits) {
  annual_profits_filled <- annual_profits %>%
    dplyr::arrange(
      scenario_name, investor_name, portfolio_name, scenario_geography, id,
      company_name, ald_sector, technology, year
    ) %>%
    dplyr::group_by(
      scenario_name, investor_name, portfolio_name, scenario_geography, id,
      company_name, ald_sector, technology
    ) %>%
    # NOTE: this assumes emissions factors stay constant after forecast and prod not continued
    tidyr::fill(
      company_id, pd, net_profit_margin, debt_equity_ratio, volatility,
      .direction = "down"
    ) %>%
    dplyr::ungroup()

  return(annual_profits_filled)
}

#' Check if values in financial data are plausible
#'
#' Checks that numeric columns hold values in acceptable ranges.
#'
#' @inheritParams check_financial_data
#'
#' @return NULL
check_valid_financial_data_values <- function(financial_data, asset_type) {
  if (any(financial_data$pd < 0 | financial_data$pd >= 1)) {
    stop("Implausibe value(s) < 0 or >= 1 for pd detected. Please check.")
  }

  if (any(financial_data$net_profit_margin <= 0 | financial_data$net_profit_margin > 1)) {
    stop("Implausibe value(s) <= 0 or > 1 for net_profit_margin detected. Please check.")
  }

  if (asset_type == "equity") {
    if (any(financial_data$debt_equity_ratio < 0)) {
      stop("Implausibe value(s) < 0 for debt_equity_ratio detected. Please check.")
    }
  } else {
    if (any(financial_data$debt_equity_ratio <= 0)) {
      stop("Implausibe value(s) <= 0 for debt_equity_ratio detected. Please check.")
    }
  }

  if (any(financial_data$volatility < 0)) {
    stop("Implausibe value(s) < 0 for volatility detected. Please check.")
  }
}

#' Wrangle results
#'
#' Function wrangles results to expected formats. List element entry `results`
#' is split into market risk results for company and portfolio level.
#'
#' @param results_list A list of results.
#' @param sensitivity_analysis_vars  String vector holding names of iteration
#'   arguments.
#'
#' @return A list of wrangled results.
wrangle_results <- function(results_list, sensitivity_analysis_vars) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  validate_data_has_expected_cols(
    data = results_list$results,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "production_shock_perc", "asset_portfolio_value",
      "tech_company_exposure", "VaR_tech_company", "tech_company_value_change",
      "company_exposure", "VaR_company", "company_value_change",
      "technology_exposure", "VaR_technology", "technology_value_change",
      "sector_exposure", "VaR_sector", "sector_value_change",
      "analysed_sectors_exposure", "VaR_analysed_sectors",
      "analysed_sectors_value_change", "portfolio_aum",
      "portfolio_value_change_perc", "portfolio_value_change", sensitivity_analysis_vars
    )
  )

  market_risk_company <- results_list$results %>%
    # ADO 2549 - select instead of relocate so that no surplus columns can sneak in
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario_geography, .data$scenario_name, .data$year_of_shock,
      .data$duration_of_shock, .data$ald_sector, .data$technology,
      .data$production_shock_perc, .data$asset_portfolio_value,
      .data$tech_company_exposure, .data$VaR_tech_company,
      .data$tech_company_value_change, .data$company_exposure,
      .data$VaR_company, .data$company_value_change, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change, .data$portfolio_aum,
      .data$portfolio_value_change_perc, .data$portfolio_value_change,
      .data$exclude, !!!rlang::syms(sensitivity_analysis_vars)
    )

  market_risk_portfolio <- results_list$results %>%
    # ADO 2549 - actively select all columns that should remain in the portfolio
    # level results, rather than unselecting some. This avoids extra columns.
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography,
      .data$scenario_name, .data$year_of_shock, .data$duration_of_shock,
      .data$ald_sector, .data$technology, .data$production_shock_perc,
      .data$asset_portfolio_value, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change, .data$portfolio_aum,
      .data$portfolio_value_change_perc, .data$portfolio_value_change,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    # ADO 2549 - all numeric variables should be unique across the CUC variables
    # running distinct all and the check afterwards ensures this is the case
    dplyr::distinct_all() %>%
    dplyr::arrange(.data$year_of_shock, .data$ald_sector, .data$technology)

  expected_loss <- results_list$expected_loss %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector,
      .data$pd, .data$PD_change, .data$lgd, .data$exposure_at_default,
      .data$expected_loss_baseline, .data$expected_loss_late_sudden,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector
    )

  annual_pd_changes_sector <- results_list$annual_pd_changes %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$year,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::summarise(
      # ADO 2312 - weight the PD change by baseline equity because this represents the original exposure better
      PD_change = weighted.mean(x = .data$PD_change, w = .data$equity_t_baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$year
    )

  overall_pd_changes_sector <- results_list$overall_pd_changes %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$term,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::summarise(
      # ADO 2312 - weight the PD change by baseline equity because this represents the original exposure better
      PD_change = weighted.mean(x = .data$PD_change, w = .data$equity_0_baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$term
    )

  return(list(
    market_risk_company = market_risk_company,
    market_risk_portfolio = market_risk_portfolio,
    expected_loss = expected_loss,
    annual_pd_changes_sector = annual_pd_changes_sector,
    overall_pd_changes_sector = overall_pd_changes_sector
  ))
}

#' Check results
#'
#' Function checks results for missings and duplicates.
#'
#' @inheritParams wrangle_results
#' @param wrangled_results_list A list of wrangled results.
#'
#' @return `wrangled_results_list`
check_results <- function(wrangled_results_list, sensitivity_analysis_vars) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  wrangled_results_list$market_risk_company %>%
    report_missings(
      name_data = "Stress test results - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "company_name", "scenario_geography",
        "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  wrangled_results_list$market_risk_portfolio %>%
    report_missings(
      name_data = "Stress test results - Portfolios level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "scenario_geography", "scenario_name",
        "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  wrangled_results_list$expected_loss %>%
    report_missings(
      name_data = "Expected loss"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "company_name", "ald_sector",
        sensitivity_analysis_vars
      )
    )

  wrangled_results_list$annual_pd_changes_sector %>%
    report_missings(
      name_data = "Annual PD changes sector"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "year", sensitivity_analysis_vars
      )
    )

  wrangled_results_list$overall_pd_changes_sector %>%
    report_missings(
      name_data = "Overall PD changes sector"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "term", sensitivity_analysis_vars
      )
    )

  return(invisible(wrangled_results_list))
}

#' Rename results
#'
#' Rename results lists entries so that results as returned to the user on
#' demand follow naming convention of exported results.
#'
#' @param results_list A list of wrangled and checked results.
#'
#' @return `result_list` with adjusted named
rename_results <- function(results_list) {
  renamed_results_list <- list(
    stress_test_results_comp = results_list$market_risk_company,
    stress_test_results_port = results_list$market_risk_portfolio,
    stress_test_results_comp_el = results_list$expected_loss,
    stress_test_results_sector_pd_changes_annual = results_list$annual_pd_changes_sector,
    stress_test_results_sector_pd_changes_overall = results_list$overall_pd_changes_sector
  )

  return(renamed_results_list)
}

#' Select required sector scenario combinations
#'
#' Function:
#' 1. Checks that for all provided sector required scenarios are available
#' 1. Filters so that only required scenarios are kept.
#'
#' @param pacta_results A tibble holding pacta results
#'
#' @return Tibble `pacta_results` holding only required sector - scenario
#'   combinations.
select_sector_scenario_combinations <- function(pacta_results) {
  pacta_results_filtered_by_sectors <- pacta_results %>%
    dplyr::filter(ald_sector %in% unique(sector_scenarios_mapping_lookup$ald_sector))

  if (nrow(pacta_results_filtered_by_sectors) == 0) {
    stop("No sectors that are supported by stresstesting are included in provided portfolio.")
  }

  sector_scenarios_mapping_lookup_filtered_by_available_sectors <- sector_scenarios_mapping_lookup %>%
    dplyr::filter(ald_sector %in% unique(pacta_results_filtered_by_sectors$ald_sector))

  scenario_availability_check_data <- pacta_results_filtered_by_sectors %>%
    dplyr::select(scenario, ald_sector) %>%
    # adding an indicator column that is reliably not NA before join
    dplyr::mutate(indicator_col = dplyr::row_number()) %>%
    dplyr::right_join(
      sector_scenarios_mapping_lookup_filtered_by_available_sectors,
      by = c("ald_sector", "scenario")
    )

  if (any(is.na(scenario_availability_check_data$indicator_col))) {

    missing_combinations <- scenario_availability_check_data %>%
      dplyr::filter(is.na(indicator_col)) %>%
      dplyr::mutate(missings = paste(scenario, ald_sector, sep = ": ")) %>%
      dplyr::pull(missings) %>%
      paste0(collapse = ", ")

    rlang::abort(c(
      "Detected missing sector scenario combination",
      x = glue::glue("Missing combinations: {missing_combinations}."),
      i = "Please check sector scenario combinations in PACTA results?."
    ))
  }

  selected_sector_scenario_combinations <- dplyr::inner_join(
    pacta_results, sector_scenarios_mapping_lookup,
    by = c("ald_sector", "scenario")
  )

  return(selected_sector_scenario_combinations)
}
