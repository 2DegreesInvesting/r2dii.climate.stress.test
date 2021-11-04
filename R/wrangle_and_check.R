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
wrangle_and_check_sector_exposures <- function(sector_exposures, asset_type) {

  asset_type <- stringr::str_to_title(asset_type)

  if (!asset_type %in% c("Bonds", "Equity", "Loans")) {
    stop("Invalid asset type", call. = FALSE )
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
#' @export
wrangle_and_check_pacta_results <- function(pacta_results, start_year, time_horizon,
                                            scenario_geography_filter, scenarios_filter,
                                            equity_market_filter) {
  wrangled_pacta_results <- pacta_results %>%
    dplyr::filter(!is.na(.data$scenario)) %>%
    check_scenario_settings(scenario_selections = scenarios_lookup) %>%
    dplyr::filter(.data$scenario %in% scenarios_lookup) %>%
    # TODO: temporary fix, remove once all scenario data is used from scenario file
    dplyr::filter(!(.data$scenario == "ETP2017_NPS" & .data$ald_sector == "Power")) %>%
    dplyr::mutate(scenario = sub(".*?_", "", scenario)) %>%
    check_portfolio_consistency(start_year = start_year) %>%
    dplyr::mutate(scenario = stringr::str_replace(.data$scenario, "NPSRTS", "NPS")) %>%
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
    ) %>%
    dplyr::distinct_all()
}

#' Wrangle financial data
#'
#' Applies custom improvements of `net_profit_margin` and does row and cols
#' selections as well as filtering. Rows that have implausible
#' net_profit_margins below or equal to 0 are removed.
#'
#' @param financial_data A data set of `financial_data`.
#' @param asset_type A string indicating if company data are for analysis for
#'   bond or equity.
#'
#' @return A prewrangled `financial_data` set.
wrangle_financial_data <- function(financial_data, asset_type) {
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
    data = data,
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
  }

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
