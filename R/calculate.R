#' Calculate annual profits
#'
#' Wrapper function to calculate annual profits.
#'
#' @inheritParams run_stress_test
#' @param asset_type String holding type of asset.
#' @param input_data_list List with project agnostic and project specific input data
#' @param scenario_to_follow_baseline Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline technology trajectories.
#' @param scenario_to_follow_ls Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden technology trajectories.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Considered timeframe for PACTA analysis.
#'
#' @return A tibble holding annual profits
calculate_annual_profits <- function(asset_type, input_data_list, scenario_to_follow_baseline,
                                     scenario_to_follow_ls, transition_scenario, start_year,
                                     end_year, time_horizon, discount_rate) {
  price_data <- input_data_list$df_price %>%
    calc_late_sudden_prices(
      baseline_scenario = scenario_to_follow_baseline,
      transition_scenario = transition_scenario,
      start_year = start_year
    )

  extended_pacta_results <- input_data_list$pacta_results %>%
    convert_power_cap_to_generation(
      capacity_factors_power = input_data_list$capacity_factors_power,
      baseline_scenario = scenario_to_follow_baseline
    ) %>%
    extend_scenario_trajectory(
      scenario_data = input_data_list$scenario_data,
      start_analysis = start_year,
      end_analysis = end_year,
      time_frame = time_horizon
    ) %>%
    set_baseline_trajectory(
      scenario_to_follow_baseline = scenario_to_follow_baseline
    ) %>%
    set_ls_trajectory(
      scenario_to_follow_ls = scenario_to_follow_ls,
      shock_scenario = transition_scenario,
      scenario_to_follow_ls_aligned = scenario_to_follow_ls,
      start_year = start_year,
      end_year = end_year,
      analysis_time_frame = time_horizon
    ) %>%
    exclude_companies(
      exclusion = input_data_list$excluded_companies,
      scenario_baseline = scenario_to_follow_baseline,
      scenario_ls = scenario_to_follow_ls
    )

  if (asset_type == "bonds") {
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker", "ald_sector", "technology")
  } else {
    merge_cols <- c("company_name", "ald_sector", "technology")
  }

  extended_pacta_results_with_financials <- extended_pacta_results %>%
    inner_join_report_drops(
      data_y = input_data_list$financial_data,
      name_x = "annual profits", name_y = "financial data",
      merge_cols = merge_cols
    ) %>%
    fill_annual_profit_cols()

  annual_profits <- extended_pacta_results_with_financials %>%
    # TODO: ADO 879 - note which companies are removed here
    join_price_data(df_prices = price_data) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate) %>%
    # TODO: ADO 879 - note rows with zero profits/NPVs will produce NaN in the Merton model
    dplyr::filter(!is.na(company_id))

  return(annual_profits)
}

#' Calculate exposure_by_technology_and_company
#'
#' Wrapper to calculate exposure_by_technology_and_company.
#'
#' @inheritParams calculate_annual_profits
#'
#' @return A tibble holding exposure_by_technology_and_company.
calculate_exposure_by_technology_and_company <- function(asset_type,
                                                         input_data_list, start_year,
                                                         scenario_to_follow_ls) {
  if (asset_type == "bonds") {
    subset_cols <- c("company_name", "corporate_bond_ticker", "ald_sector", "technology", "pd")
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker", "ald_sector", "technology")
  } else {
    subset_cols <- c("company_name", "ald_sector", "technology", "pd")
    merge_cols <- c("company_name", "ald_sector", "technology")
  }

  financial_data_subset <- input_data_list$financial_data %>%
    dplyr::select(!!!rlang::syms(subset_cols)) %>%
    report_all_duplicate_kinds(
      cols = names(financial_data_subset)
    )

  exposure_by_technology_and_company <- input_data_list$pacta_results %>%
   dplyr::filter(
     .data$year == .env$start_year,
     .data$scenario %in% .env$scenario_to_follow_ls
   ) %>%
   inner_join_report_drops(
     data_y = financial_data_subset,
     name_x = "plan carsten", name_y = "financial data",
     merge_cols = merge_cols
   ) %>%
   # TODO: ADO 879 - note which companies are removed here, what to do with entries that have NAs for pd?
   dplyr::select(
     investor_name, portfolio_name, company_name, ald_sector, technology,
     scenario_geography, year, plan_carsten, plan_sec_carsten, term, pd
   ) %>%
   report_all_duplicate_kinds(
     cols = names(exposure_by_technology_and_company)
   )

  return(exposure_by_technology_and_company)
}
