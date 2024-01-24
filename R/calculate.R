#' Calculate transition shock trajectory
#'
#' @inheritParams validate_input_values
#' @inheritParams report_company_drops
#' @param input_data_list List with project agnostic and project specific input data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Considered timeframe for PACTA analysis.
#'
#' @return A tibble holding annual profits
calculate_trisk_trajectory <- function(input_data_list,
                                       baseline_scenario,
                                       target_scenario,
                                       transition_scenario,
                                       start_year,
                                       end_year,
                                       time_horizon,
                                       log_path) {

  production_data <- input_data_list$production_data %>%
    set_baseline_trajectory(
      baseline_scenario = baseline_scenario
    ) %>%
    set_trisk_trajectory(
      target_scenario = target_scenario,
      shock_scenario = transition_scenario,
      target_scenario_aligned = target_scenario,
      start_year = start_year,
      end_year = end_year,
      analysis_time_frame = time_horizon,
      log_path = log_path
    )

  price_data <- input_data_list$df_price %>%
    calc_scenario_prices(
      baseline_scenario = baseline_scenario,
      target_scenario = target_scenario,
      transition_scenario = transition_scenario,
      start_year = start_year
    )

  full_trajectory <- production_data %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = c("company_id")
    ) %>%
    stop_if_empty(data_name = "Production data joined with Financial data") %>%
    fill_annual_profit_cols()

  full_trajectory <- full_trajectory %>%
    join_price_data(df_prices = price_data)

  return(full_trajectory)
}

#' Calculate litigation shock trajectory
#'
#' @inheritParams validate_input_values
#' @inheritParams report_company_drops
#' @param input_data_list List with project agnostic and project specific input data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param litigation_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock`, `duration_of_shock`, `scc` and `exp_share_damages_paid`
#' @param start_year Numeric, holding start year of analysis.
#' @param end_year Numeric, holding end year of analysis.
#' @param time_horizon Considered timeframe for PACTA analysis.
#'
#' @return A tibble holding annual profits
calculate_lrisk_trajectory <- function(input_data_list,
                                       baseline_scenario,
                                       target_scenario,
                                       litigation_scenario,
                                       start_year,
                                       end_year,
                                       time_horizon,
                                       log_path) {
  production_data <- input_data_list$production_data %>%
    set_baseline_trajectory(
      baseline_scenario = baseline_scenario
    ) %>%
    # we currently assume that production levels and emission factors of
    # misaligned company-ald_business_unit combinations are forced onto the target
    # scenario trajectory directly after the litigation shock.
    # This may not be perfectly realistic and may be refined in the future.
    # TODO: we need to decide how to handle low carbon technologies.
    # currently they are exempt from liabilities of not building out enough.
    # this may be realistic, but misaligned ones should not switch their
    # production to the target trajectory. This would lead to unrealistic jumps
    # in buildout and the litigation risk model should not require solving
    # for the scenario.
    set_litigation_trajectory(
      litigation_scenario = target_scenario,
      shock_scenario = litigation_scenario,
      litigation_scenario_aligned = target_scenario,
      start_year = start_year,
      end_year = end_year,
      analysis_time_frame = time_horizon_lookup,
      log_path = log_path
    ) %>%
    # TODO: put the mutate into an aptly named function
    dplyr::mutate(
      actual_emissions = .data$late_sudden * .data$emission_factor,
      allowed_emissions = !!rlang::sym(target_scenario) * .data$emission_factor,
      overshoot_emissions = dplyr::if_else(
        .data$actual_emissions - .data$allowed_emissions < 0,
        0,
        .data$actual_emissions - .data$allowed_emissions
      )
    )

  # TODO: decide if a slow change in price trajectory is needed...
  # For now, we assume that we just have the standard prices which are renamed to be able to use functions
  price_data <- input_data_list$df_price %>%
    dplyr::rename(
      Baseline_price = !!rlang::sym(glue::glue("price_{baseline_scenario}")),
      late_sudden_price = !!rlang::sym(glue::glue("price_{target_scenario}"))
    )

  merge_cols <- c("company_id" = "company_id")

  full_trajectory <- production_data %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = merge_cols
    ) %>%
    stop_if_empty(data_name = "Production data joined with Financial data") %>%
    fill_annual_profit_cols()

  full_trajectory <- full_trajectory %>%
    join_price_data(df_prices = price_data)

  return(full_trajectory)
}

#' Calculate annual profits
#'
#' Wrapper function to calculate discounted annual profits and terminal value.
#'
#' @inheritParams validate_input_values
#' @inheritParams report_company_drops
#' @param data data frame containing the full trajectory company data
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#' @param shock_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param end_year Numeric, holding end year of analysis.
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the `end_year` in the DCF.
#'
#' @return A tibble holding annual profits
calculate_annual_profits <- function(data,
                                     baseline_scenario,
                                     shock_scenario,
                                     end_year,
                                     discount_rate,
                                     growth_rate,
                                     log_path) {
  data <- data %>%
    dividend_discount_model(discount_rate = discount_rate) %>%
    calculate_terminal_value(
      end_year = end_year,
      growth_rate = growth_rate,
      discount_rate = discount_rate,
      baseline_scenario = baseline_scenario,
      shock_scenario = shock_scenario
    )

  return(data)
}

#' Calculate annual profits after payout of settlement in lrisk
#'
#' @param data data frame containing the full trajectory company data
#' @param scc Numeric. Social cost of carbon per excess ton of CO2 emitted. This
#'   is the price for each surplus ton of CO2 that goes into the calculation of
#'   the carbon liability of a company.
#' @param exp_share_damages_paid Numeric. Ratio that defines the expected share
#'   of the calculated social cost of carbon that is considered in the liability.
#' @param settlement_factor Catch all factor (ratio) that can be used to adjust
#'   the expected payout of the settlement due to further data gaps. Set to 1 by
#'   default.
#' @param shock_year Numeric, year of the litigation event
#'
#' @return A tibble holding annual profits
subtract_settlement <- function(data,
                                scc,
                                exp_share_damages_paid,
                                settlement_factor,
                                shock_year) {
  data <- data %>%
    dplyr::mutate(
      scc_liability =
        .data$overshoot_emissions * .env$scc *
          .env$exp_share_damages_paid
    ) %>%
    dplyr::group_by(.data$company_name, .data$ald_sector, .data$ald_business_unit) %>%
    dplyr::mutate(
      settlement = sum(.data$scc_liability, na.rm = TRUE) * .env$settlement_factor
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      net_profits_ls = dplyr::if_else(
        .data$year == .env$shock_year,
        .data$net_profits_ls - .data$settlement,
        .data$net_profits_ls
      )
    )

  return(data)
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
      Baseline_price = NA_real_,
      late_sudden_price = NA_real_,
      production_compensation = NA_real_
    )

  data <- data %>%
    dplyr::bind_rows(terminal_value) %>%
    dplyr::arrange(
      .data$company_id, .data$scenario_geography, .data$company_name, .data$ald_sector,
      .data$ald_business_unit, .data$year
    )

  return(data)
}
