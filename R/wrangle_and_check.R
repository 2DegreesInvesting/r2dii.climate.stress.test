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
wrangle_and_check_sector_exposures_eq_cb <- function(sector_exposures, asset_type) {
  if (!asset_type %in% c("Bonds", "Equity")) {
    stop("Can only wrangle dataset for asset types bonds and equity.")
  }

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
#' @param financial_data A data set of `financal_data`.
#' @param start_year String holding start year of analysis.
#'
#' @return A prewrangled `financal_data` set.
wrangle_financial_data <- function(financial_data, start_year) {
  financial_data <- financial_data %>%
    dplyr::mutate(net_profit_margin = profit_margin_preferred) %>%
    # TODO: logic unclear thus far
    dplyr::mutate(
      net_profit_margin = dplyr::case_when(
        net_profit_margin < 0 & dplyr::between(profit_margin_unpreferred, 0, 1) ~ profit_margin_unpreferred,
        net_profit_margin < 0 & profit_margin_unpreferred < 0 ~ 0,
        net_profit_margin < 0 & profit_margin_unpreferred > 1 ~ 0,
        net_profit_margin > 1 & dplyr::between(profit_margin_unpreferred, 0, 1) ~ profit_margin_unpreferred,
        net_profit_margin > 1 & profit_margin_unpreferred > 1 ~ 1,
        net_profit_margin > 1 & profit_margin_unpreferred < 0 ~ 1,
        TRUE ~ net_profit_margin
      )
    ) %>%
    dplyr::filter(net_profit_margin > 0) %>%
    dplyr::select(-c(profit_margin_preferred, profit_margin_unpreferred)) %>%
    dplyr::rename(
      debt_equity_ratio = leverage_s_avg,
      volatility = asset_volatility_s_avg
    ) %>%
    # ADO 879 - remove year and production/EFs to simplify joins that do not need yearly variation yet
    dplyr::filter(.data$year == start_year) %>%
    dplyr::select(
      -c(
        .data$year, .data$ald_production_unit, .data$ald_production,
        .data$ald_emissions_factor_unit, .data$ald_emissions_factor
      )
    )
  # TODO: any logic/bounds needed for debt/equity ratio and volatility?

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
