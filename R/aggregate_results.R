#' Aggregate results
#'
#' Function aggregates results to the expected levels. For market risk, the most
#' granular level company-technology value changes are aggregated to (1) the
#' company level and then (2) the portfolio level with splits by technology,
#' sector, all analyzed sectors and overall portfolio impact. For the credit
#' risk results, the most granular level already starts at company level. Here,
#' we additionally aggregate to the portfolio level, providing a sector split of
#' the credit risk impact on the portfolio. In all cases, the aggregation to the
#' portfolio level is done weighting the holdings in the given companies by
#' exposure. List element entry `results` is split into market risk and credit
#' risk results for company and portfolio level.
#'
#' @param results_list A list of results.
#' @param sensitivity_analysis_vars  String vector holding names of iteration
#'   arguments.
#' @param iter_var String vector holding the variable over which to iterate in
#'   sensitivity analysis
#'
#' @return A list including basic and aggregated results.
aggregate_results <- function(results_list, sensitivity_analysis_vars, iter_var) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  # Aggregate Crispy Results -----

  crispy_output <- results_list$company_technology_npv %>%
    dplyr::inner_join(
      results_list$company_pd_changes_overall,
      by = c(
        "scenario_name", "scenario_geography", "company_name", "ald_sector",
        "baseline_scenario_arg", "shock_scenario_arg", "lgd_arg",
        "risk_free_rate_arg", "discount_rate_arg", "growth_rate_arg",
        "div_netprofit_prop_coef_arg", "shock_year_arg", "fallback_term_arg",
        "use_company_terms_arg"
      )
    )

  crispy_output <- crispy_output %>%
    dplyr::mutate(roll_up_type = "equity_ownership") %>%
    dplyr::rename(
      sector = .data$ald_sector,
      business_unit = .data$technology,
      baseline_scenario = .data$baseline_scenario_arg,
      shock_scenario = .data$shock_scenario_arg,
      lgd = .data$lgd_arg,
      discount_rate = .data$discount_rate_arg,
      dividend_rate = .data$div_netprofit_prop_coef_arg,
      growth_rate = .data$growth_rate_arg,
      shock_year = .data$shock_year_arg,
      net_present_value_baseline = .data$total_disc_npv_baseline,
      net_present_value_shock = .data$total_disc_npv_ls,
      pd_baseline = .data$PD_baseline,
      pd_shock = .data$PD_late_sudden
    ) %>%
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      pd_difference = .data$pd_shock - .data$pd_baseline
    ) %>%
    dplyr::select(
      .data$company_name, .data$sector, .data$business_unit,
      .data$roll_up_type, .data$scenario_geography,  .data$baseline_scenario,
      .data$shock_scenario, .data$lgd, .data$risk_free_rate, .data$discount_rate,
      .data$dividend_rate, .data$growth_rate, .data$shock_year,
      .data$net_present_value_baseline, .data$net_present_value_shock,
      .data$net_present_value_difference, .data$term, .data$pd_baseline,
      .data$pd_shock, .data$pd_difference
    )

  return(list(
    company_trajectories = results_list$company_trajectories,
    crispy_output = crispy_output
  ))
}
