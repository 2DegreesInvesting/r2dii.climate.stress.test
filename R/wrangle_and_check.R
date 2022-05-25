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
    name_data = "sector exposures",
    throw_error = TRUE
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
#' @inheritParams process_pacta_results
#' @param pacta_results Results from PACTA analysis.
#'
#' @return Wrangled `pacta_results.`
wrangle_and_check_pacta_results <- function(pacta_results, start_year, time_horizon) {
  wrangled_pacta_results <- pacta_results %>%
    select_sector_scenario_combinations() %>%
    dplyr::mutate(scenario = sub(".*?_", "", scenario)) %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      tidyr::nesting(!!!rlang::syms(nesting_vars_lookup))
    ) %>%
    dplyr::mutate(plan_tech_prod = dplyr::if_else(is.na(.data$plan_tech_prod), 0, .data$plan_tech_prod))

  return(wrangled_pacta_results)
}

#' Check financial data
#'
#' Applies sanity checks to financial data. Also remove column
#' corporate_bond_ticker if `asset_type` is not bonds.
#'
#' @param financial_data A data set of `financial_data`.
#' @param asset_type A string indicating if company data are for analysis for
#'   bond or equity.
#' @param interactive_mode If TRUE the, more verbose, interactive mode is used.
#'
#' @return Returns prewrangled `financial_data` invisibly.
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
check_financial_data <- function(financial_data, asset_type,
                                 interactive_mode = FALSE) {
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
    financial_data <- financial_data %>%
      dplyr::filter(!is.na(.data$corporate_bond_ticker)) %>%
      check_company_ticker_mapping()
  }

  report_missings(
    data = financial_data,
    name_data = "Financial Data",
    throw_error = TRUE
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

  if (interactive_mode) {
    message("Financial data validated successfully.")
  }

  return(invisible(financial_data))
}

#' Check company term values for plausibility
#'
#' @param data A tibble holding company_terms data.
#' @param interactive_mode If TRUE the, more verbose, interactive mode is used.
#'
#' @return Returns data invisibly.
#' @export
check_company_terms <- function(data, interactive_mode = FALSE) {
  not_na_terms <- data %>%
    dplyr::filter(!is.na(.data$term)) %>%
    dplyr::pull(.data$term)

  if (any(not_na_terms < 1)) {
    rlang::abort(c(
      "Must not provide terms below 1",
      x = glue::glue("Identified terms below."),
      i = "Please check company_terms.csv file."
    ))
  }

  if (!all(not_na_terms %% 1 == 0)) {
    rlang::abort(c(
      "Terms must be provided as whole numbers.",
      x = glue::glue("Identified terms that are not whole numbers."),
      i = "Please check company_terms.csv file."
    ))
  }

  if (interactive_mode) {
    n_terms_bigger_5 <- not_na_terms[not_na_terms > 5]

    if (length(n_terms_bigger_5) > 0) {
      message(paste("Identified", length(n_terms_bigger_5), "companies with term > 5. Terms will be capped."))
    }

    message("Company - term data validated successfully.")
  }

  return(invisible(data))
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

  # value changes -----------------------------------------------------------
  validate_data_has_expected_cols(
    data = results_list$company_value_changes,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "asset_portfolio_value",
      "tech_company_exposure", "VaR_tech_company", "tech_company_value_change",
      "company_exposure", "VaR_company", "company_value_change",
      "technology_exposure", "VaR_technology", "technology_value_change",
      "sector_exposure", "VaR_sector", "sector_value_change",
      "analysed_sectors_exposure", "VaR_analysed_sectors",
      "analysed_sectors_value_change", sensitivity_analysis_vars
    )
  )

  company_value_changes <- results_list$company_value_changes %>%
    # ADO 2549 - select instead of relocate so that no surplus columns can sneak in
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario_geography, .data$scenario_name, .data$year_of_shock,
      .data$duration_of_shock, .data$ald_sector, .data$technology,
      .data$asset_portfolio_value,
      .data$tech_company_exposure, .data$VaR_tech_company,
      .data$tech_company_value_change, .data$company_exposure,
      .data$VaR_company, .data$company_value_change, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::rename(
      company_technology_exposure = .data$tech_company_exposure,
      company_technology_percent_value_change = .data$VaR_tech_company,
      company_technology_absolute_value_change = .data$tech_company_value_change,
      company_exposure = .data$company_exposure,
      company_percent_value_change = .data$VaR_company,
      company_absolute_value_change = .data$company_value_change,
      technology_exposure = .data$technology_exposure,
      technology_percent_value_change = .data$VaR_technology,
      technology_absolute_value_change = .data$technology_value_change,
      sector_exposure = .data$sector_exposure,
      sector_percent_value_change = .data$VaR_sector,
      sector_absolute_value_change = .data$sector_value_change,
      analysed_portfolio_exposure = .data$analysed_sectors_exposure,
      analysed_portfolio_percent_value_change = .data$VaR_analysed_sectors,
      analysed_portfolio_absolute_value_change = .data$analysed_sectors_value_change
    )

  portfolio_value_changes <- results_list$company_value_changes %>%
    # ADO 2549 - actively select all columns that should remain in the portfolio
    # level results, rather than unselecting some. This avoids extra columns.
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography,
      .data$scenario_name, .data$year_of_shock, .data$duration_of_shock,
      .data$ald_sector, .data$technology,
      .data$asset_portfolio_value, .data$technology_exposure,
      .data$VaR_technology, .data$technology_value_change,
      .data$sector_exposure, .data$VaR_sector, .data$sector_value_change,
      .data$analysed_sectors_exposure, .data$VaR_analysed_sectors,
      .data$analysed_sectors_value_change,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    # ADO 2549 - all numeric variables should be unique across the CUC variables
    # running distinct all and the check afterwards ensures this is the case
    dplyr::distinct_all() %>%
    dplyr::arrange(.data$year_of_shock, .data$ald_sector, .data$technology) %>%
    dplyr::rename(
      technology_exposure = .data$technology_exposure,
      technology_percent_value_change = .data$VaR_technology,
      technology_absolute_value_change = .data$technology_value_change,
      sector_exposure = .data$sector_exposure,
      sector_percent_value_change = .data$VaR_sector,
      sector_absolute_value_change = .data$sector_value_change,
      analysed_portfolio_exposure = .data$analysed_sectors_exposure,
      analysed_portfolio_percent_value_change = .data$VaR_analysed_sectors,
      analysed_portfolio_absolute_value_change = .data$analysed_sectors_value_change
    )

  # expected loss -----------------------------------------------------------
  company_expected_loss <- results_list$company_expected_loss %>%
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
    ) %>%
    dplyr::rename(
      pd_baseline = .data$pd,
      pd_change_shock = .data$PD_change,
      expected_loss_shock = .data$expected_loss_late_sudden
    )

  # pd changes --------------------------------------------------------------
  company_pd_changes_annual <- results_list$company_pd_changes_annual %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$year,
      .data$PD_change, !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$year
    ) %>%
    dplyr::rename(
      pd_change_shock = .data$PD_change
    )

  portfolio_pd_changes_annual <- results_list$company_pd_changes_annual %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$year,
      .data$PD_change, .data$PD_change_sector,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$year
    ) %>%
    dplyr::rename(
      pd_change_shock = .data$PD_change,
      pd_change_sector_shock = .data$PD_change_sector
    ) %>%
    dplyr::distinct_all()


  company_pd_changes_overall <- results_list$company_pd_changes_overall %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$term,
      .data$PD_change, !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$term
    ) %>%
    dplyr::rename(
      pd_change_shock = .data$PD_change
    )

  portfolio_pd_changes_overall <- results_list$company_pd_changes_overall %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$term,
      .data$PD_change, .data$PD_change_sector,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector, .data$term
    ) %>%
    dplyr::rename(
      pd_change_shock = .data$PD_change,
      pd_change_sector_shock = .data$PD_change_sector
    ) %>%
    dplyr::distinct_all()

  # company trajectories ----------------------------------------------------
  company_trajectories <- results_list$company_trajectories %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_name,
      .data$company_name, .data$year, .data$scenario_geography, .data$ald_sector,
      .data$technology, .data$plan_tech_prod, .data$phase_out, .data$baseline,
      .data$scen_to_follow_aligned, .data$late_sudden,
      .data$scenario_change_aligned, .data$company_id, .data$pd,
      .data$net_profit_margin, .data$debt_equity_ratio, .data$volatility,
      .data$Baseline_price, .data$late_sudden_price, .data$net_profits_baseline,
      .data$net_profits_ls, .data$discounted_net_profit_baseline,
      .data$discounted_net_profit_ls, !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::rename(
      production_plan_company_technology = .data$plan_tech_prod,
      # TODO: add once ADO3530 is merged
      # direction_of_target = .data$direction,
      production_baseline_scenario = .data$baseline,
      production_target_scenario = .data$scen_to_follow_aligned,
      production_shock_scenario = .data$late_sudden,
      production_change_target_scenario = .data$scenario_change_aligned,
      price_baseline_scenario = .data$Baseline_price,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    )

  return(list(
    company_value_changes = company_value_changes,
    portfolio_value_changes = portfolio_value_changes,
    company_expected_loss = company_expected_loss,
    company_pd_changes_annual = company_pd_changes_annual,
    portfolio_pd_changes_annual = portfolio_pd_changes_annual,
    company_pd_changes_overall = company_pd_changes_overall,
    portfolio_pd_changes_overall = portfolio_pd_changes_overall,
    company_trajectories = company_trajectories
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

  # value changes -----------------------------------------------------------
  wrangled_results_list$company_value_changes %>%
    report_missings(
      name_data = "Value changes - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "company_name", "scenario_geography",
        "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  wrangled_results_list$portfolio_value_changes %>%
    report_missings(
      name_data = "Value changes - Portfolios level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "scenario_geography", "scenario_name",
        "year_of_shock", "duration_of_shock", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    )

  # expected loss -----------------------------------------------------------
  wrangled_results_list$company_expected_loss %>%
    report_missings(
      name_data = "Expected loss - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "company_name", "ald_sector",
        sensitivity_analysis_vars
      )
    )

  # pd changes --------------------------------------------------------------
  wrangled_results_list$company_pd_changes_annual %>%
    report_missings(
      name_data = "Annual PD changes - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "year", "company_name", sensitivity_analysis_vars
      )
    )

  wrangled_results_list$portfolio_pd_changes_annual %>%
    report_missings(
      name_data = "Annual PD changes - Portfolio level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "year", "company_name", sensitivity_analysis_vars
      )
    )

  wrangled_results_list$company_pd_changes_overall %>%
    report_missings(
      name_data = "Overall PD changes - Company level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "term", "company_name", sensitivity_analysis_vars
      )
    )

  wrangled_results_list$portfolio_pd_changes_overall %>%
    report_missings(
      name_data = "Overall PD changes - Portfolio level"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "scenario_name", "scenario_geography", "investor_name", "portfolio_name",
        "ald_sector", "term", "company_name", sensitivity_analysis_vars
      )
    )

  # company trajectories ----------------------------------------------------
  wrangled_results_list$company_trajectories %>%
    # ADO 3112 - the last year contains the terminal value, which has no
    # production values hence that year contains multiple NAs and is ignored
    # here. Since this also affects the number of rows, we exclude the terminal
    # value year in the check for expected missingness already
    dplyr::filter(.data$year != max(.data$year, na.rm = TRUE)) %>%
    check_expected_missings() %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "investor_name", "portfolio_name", "company_name", "year",
        "scenario_geography", "ald_sector", "technology", sensitivity_analysis_vars
      )
    ) %>%
    # not considering those two variables when checking for missings because
    # acceptable missing pattern is checked in ADO 4919
    dplyr::select(-c(.data$production_plan_company_technology, .data$production_change_target_scenario)) %>%
    report_missings(
      name_data = "Company Trajectories"
    )

  return(invisible(wrangled_results_list))
}

#' Select required sector scenario combinations
#'
#' Function:
#' 1. Checks that for all supported sectors contained in `pacta_results`
#' required scenarios are available.
#' 1. Filters so that only required scenarios are kept.
#'
#' @param pacta_results A tibble holding pacta results.
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
      dplyr::filter(is.na(.data$indicator_col)) %>%
      dplyr::mutate(missings = paste(.data$scenario, .data$ald_sector, sep = ": ")) %>%
      dplyr::pull(.data$missings) %>%
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


#' Fill missing terms with fallback_term
#'
#' Also throws informative message on number of filled values.
#'
#' @param data A tibble holding at least column `term`.
#' @param fallback_term Numeric, holding fallback term.
#'
#' @return Tibble `data `
fill_na_terms <- function(data, fallback_term) {
  n_companies_with_na_term <- data %>%
    dplyr::filter(is.na(term)) %>%
    nrow()

  if (n_companies_with_na_term > 0) {
    message(paste("Using fallback term", fallback_term, "for", n_companies_with_na_term, "companies."))

    data <- data %>%
      dplyr::mutate(term = dplyr::if_else(is.na(.data$term), as.double(fallback_term), .data$term))
  }

  return(data)
}

#' Cap terms
#'
#' Caps terms to maximum of 5 (years). The value 5 was chosen as for longer
#' time frames reliability of model deteriorates. Informative message on number
#' of affected companies is thrown.
#'
#' @param data A tibble holding at least column `term`.
#'
#' @return Tibble `data` with capped term.
cap_terms <- function(data) {
  n_terms_bigger_5 <- data %>%
    dplyr::filter(.data$term > 5) %>%
    nrow()

  if (n_terms_bigger_5 > 0) {
    message(paste("Capping term values to 5 for", n_terms_bigger_5, "companies."))

    data <- data %>%
      dplyr::mutate(term = dplyr::if_else(.data$term > 5, 5, .data$term))
  }

  return(data)
}

add_term_to_trajectories <- function(annual_profits, pacta_results) {
  distinct_company_terms <- pacta_results %>%
    dplyr::select(company_name, term) %>%
    dplyr::distinct_all()

  report_duplicates(data = distinct_company_terms, cols = names(distinct_company_terms))

  annual_profits_with_term <- annual_profits %>%
    dplyr::inner_join(distinct_company_terms, by = c("company_name"))

  return(annual_profits_with_term)
}
