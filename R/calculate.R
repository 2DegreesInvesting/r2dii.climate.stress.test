calculate_annual_profits <- function(asset_type, input_data_list, scenario_to_follow_baseline,
                                    scenario_to_follow_ls, transition_scenario, start_year,
                                    end_year, time_horizon, discount_rate) {

  df_prices <- input_data_list$df_price %>%
    calc_late_sudden_prices(
      baseline_scenario = scenario_to_follow_baseline,
      transition_scenario = transition_scenario,
      start_year = start_year
    )

  annual_profits <- input_data_list$pacta_results %>%
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
    ) %>%
    inner_join_report_drops(
      data_y = input_data_list$financial_data,
      name_x = "annual profits", name_y = "financial data",
      merge_cols = c("company_name", "ald_sector", "technology")
    ) %>%
    fill_annual_profit_cols() %>%
    # TODO: ADO 879 - note which companies are removed here
    join_price_data(df_prices = df_prices) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate) %>%
    dplyr::filter(!is.na(company_id))
}
