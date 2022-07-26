#' Calculate annual profits
#'
#' Wrapper function to calculate annual profits.
#'
#' @inheritParams validate_input_values
#' @inheritParams report_company_drops
#' @param asset_type String holding type of asset.
#' @param input_data_list List with project agnostic and project specific input data
#' @param scenario_to_follow_baseline Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline technology trajectories.
#' @param scenario_to_follow_shock Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden technology trajectories.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Considered timeframe for PACTA analysis.
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the `end_year` in the DCF.
#'
#' @return A tibble holding annual profits
calculate_annual_profits <- function(asset_type, input_data_list, scenario_to_follow_baseline,
                                     scenario_to_follow_shock, transition_scenario, start_year,
                                     end_year, time_horizon, discount_rate,
                                     growth_rate, log_path) {
  price_data <- input_data_list$df_price %>%
    calc_scenario_prices(
      baseline_scenario = scenario_to_follow_baseline,
      shock_scenario = scenario_to_follow_shock,
      transition_scenario = transition_scenario,
      start_year = start_year
    )

  extended_pacta_results <- input_data_list$pacta_results %>%
    extend_scenario_trajectory(
      scenario_data = input_data_list$scenario_data,
      start_analysis = start_year,
      end_analysis = end_year,
      time_frame = time_horizon,
      target_scenario = scenario_to_follow_shock
    ) %>%
    set_baseline_trajectory(
      scenario_to_follow_baseline = scenario_to_follow_baseline
    ) %>%
    # some companies dropped, document
    set_ls_trajectory(
      scenario_to_follow_ls = scenario_to_follow_shock,
      shock_scenario = transition_scenario,
      scenario_to_follow_ls_aligned = scenario_to_follow_shock,
      start_year = start_year,
      end_year = end_year,
      analysis_time_frame = time_horizon,
      log_path = log_path
    )

  if (asset_type == "bonds") {
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker")
  } else {
    merge_cols <- c("company_name")
  }

  extended_pacta_results_with_financials <- extended_pacta_results %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = merge_cols
    ) %>%
    fill_annual_profit_cols()

  annual_profits <- extended_pacta_results_with_financials %>%
    join_price_data(df_prices = price_data) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate) %>%
    # TODO: ADO 879 - note rows with zero profits/NPVs will produce NaN in the Merton model
    dplyr::filter(!is.na(company_id))

  annual_profits <- annual_profits %>%
    calculate_terminal_value(
      end_year = end_year,
      growth_rate = growth_rate,
      discount_rate = discount_rate,
      baseline_scenario = scenario_to_follow_baseline,
      shock_scenario = scenario_to_follow_shock
    )

  return(annual_profits)
}

#' Calculate exposure_by_technology_and_company
#'
#' Wrapper to calculate exposure_by_technology_and_company.
#'
#' @inheritParams calculate_annual_profits
#' @inheritParams report_company_drops
#'
#' @return A tibble holding exposure_by_technology_and_company.
calculate_exposure_by_technology_and_company <- function(asset_type,
                                                         input_data_list,
                                                         start_year,
                                                         time_horizon,
                                                         scenario_to_follow_shock,
                                                         log_path) {
  if (asset_type == "bonds") {
    subset_cols <- c("company_name", "corporate_bond_ticker", "pd")
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker")
  } else {
    subset_cols <- c("company_name", "pd")
    merge_cols <- c("company_name")
  }

  financial_data_subset <- input_data_list$financial_data %>%
    dplyr::select(!!!rlang::syms(subset_cols)) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = names(.)
    )

  exposure_by_technology_and_company <- input_data_list$pacta_results %>%
    dplyr::filter(
      .data$year == .env$start_year + .env$time_horizon,
      .data$scenario %in% .env$scenario_to_follow_shock
    ) %>%
    dplyr::inner_join(
      y = financial_data_subset,
      by = merge_cols
    ) %>%
    dplyr::select(
      investor_name, portfolio_name, company_name, ald_sector, technology,
      scenario_geography, year, plan_carsten, plan_sec_carsten, term, pd
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = names(.)
    )

  n_companies_pre <- length(unique(exposure_by_technology_and_company$company_name))

  exposure_by_technology_and_company_filtered <- exposure_by_technology_and_company %>%
    dplyr::filter(.data$plan_carsten > 0)

  n_companies_post <- length(unique(exposure_by_technology_and_company_filtered$company_name))

  if (n_companies_pre > n_companies_post) {
    percent_loss <- (n_companies_pre - n_companies_post) * 100 / n_companies_pre
    affected_companies <- sort(
      setdiff(
        exposure_by_technology_and_company$company_name,
        exposure_by_technology_and_company_filtered$company_name
      )
    )
    paste_write(
      format_indent_1(), "When filtering out holdings with exposures of 0 value, dropped rows for",
      n_companies_pre - n_companies_post, "out of", n_companies_pre, "companies",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
  }

  return(exposure_by_technology_and_company_filtered)
}

calculate_terminal_value <- function(data,
                                     end_year,
                                     growth_rate,
                                     discount_rate,
                                     baseline_scenario,
                                     shock_scenario) {
  # the calculation follows the formula described in the 2DII paper "Limited
  # Visibility", available under https://2degrees-investing.org/resource/limited-visibility-the-current-state-of-corporate-disclosure-on-long-term-risks/
  terminal_value <- data %>%
    dplyr::filter(.data$year == .env$end_year) %>%
    dplyr::mutate(
      year = .env$end_year + 1,
      net_profits_baseline = .data$net_profits_baseline * (1 + .env$growth_rate),
      net_profits_ls = .data$net_profits_ls * (1 + .env$growth_rate),
      discounted_net_profit_baseline = .data$net_profits_baseline /
        (.env$discount_rate - .env$growth_rate),
      discounted_net_profit_ls = .data$net_profits_ls /
        (.env$discount_rate - .env$growth_rate)
    ) %>%
    # ADO3112: All columns that reflect a change over time are set to NA, as
    # they cannot be extrapolated from the start_year to end_year period. All
    # columns that are time invariant are kept.
    dplyr::mutate(
      !!rlang::sym(baseline_scenario) := NA_real_,
      !!rlang::sym(shock_scenario) := NA_real_,
      baseline = NA_real_,
      scen_to_follow_aligned = NA_real_,
      late_sudden = NA_real_,
      scenario_change_aligned = NA_real_,
      Baseline_price = NA_real_,
      late_sudden_price = NA_real_,
      production_compensation = NA_real_
    )

  data <- data %>%
    dplyr::bind_rows(terminal_value) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$id,
      .data$scenario_geography, .data$company_name, .data$ald_sector,
      .data$technology, .data$year
    )

  return(data)
}
