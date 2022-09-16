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
#' @param risk_type String that is either lrisk or trisk
#'
#' @return A list including basic and aggregated results.
aggregate_results <- function(results_list, sensitivity_analysis_vars, iter_var, risk_type) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  # Aggregate Crispy Results -----

  merge_by_cols <- c(
    "investor_name", "portfolio_name", "scenario_name", "scenario_geography",
    "company_name", "ald_sector", "asset_type_arg", "baseline_scenario_arg",
    "shock_scenario_arg", "lgd_arg", "risk_free_rate_arg", "discount_rate_arg",
    "growth_rate_arg", "div_netprofit_prop_coef_arg", "shock_year_arg",
    "fallback_term_arg", "use_company_terms_arg"
  )

  if (risk_type == "lrisk") {
    lrisk_additional <- c("scc_arg", "settlement_factor_arg", "exp_share_damages_paid_arg")
    merge_by_cols <- c(merge_by_cols, lrisk_additional)
  }

  crispy_output <- results_list$company_technology_npv %>%
    dplyr::inner_join(
      results_list$company_pd_changes_overall,
      by = merge_by_cols
    )

  crispy_output <- crispy_output %>%
    dplyr::mutate(
      roll_up_type = dplyr::if_else(
        .data$asset_type_arg == "bonds",
        "financial_control",
        "equity_ownership"
      )
    )

  crispy_output <- crispy_output %>%
    dplyr::rename(
      sector = .data$ald_sector,
      business_unit = .data$technology,
      calculation_type = .data$asset_type_arg,
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
    )

  if (risk_type == "lrisk") {
    crispy_output <- crispy_output %>%
      dplyr::inner_join(results_list$company_trajectories %>%
        dplyr::select(.data$id, .data$company_name, .data$company_is_litigated) %>%
        dplyr::distinct_all(),
      by = c("id", "company_name")
      )

    crispy_output <- crispy_output %>%
      dplyr::rename(
        scc = .data$scc_arg,
        settlement_factor = .data$settlement_factor_arg,
        exp_share_damages_paid = .data$exp_share_damages_paid_arg
      )
  }

  crispy_output <- crispy_output %>%
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      pd_difference = .data$pd_shock - .data$pd_baseline
    )

  if (risk_type == "lrisk") {
    crispy_output <- crispy_output %>%
      dplyr::select(
        .data$company_name, .data$sector, .data$business_unit,
        .data$roll_up_type, .data$scenario_geography, .data$calculation_type,
        .data$baseline_scenario, .data$shock_scenario, .data$lgd,
        .data$risk_free_rate, .data$discount_rate, .data$dividend_rate,
        .data$growth_rate, .data$scc, .data$settlement_factor, .data$exp_share_damages_paid,
        .data$shock_year, .data$net_present_value_baseline,
        .data$net_present_value_shock, .data$net_present_value_difference,
        .data$term, .data$pd_baseline, .data$pd_shock, .data$pd_difference,
        .data$company_is_litigated
      )
  } else {
    crispy_output <- crispy_output %>%
      dplyr::select(
        .data$company_name, .data$sector, .data$business_unit,
        .data$roll_up_type, .data$scenario_geography, .data$calculation_type,
        .data$baseline_scenario, .data$shock_scenario, .data$lgd,
        .data$risk_free_rate, .data$discount_rate, .data$dividend_rate,
        .data$growth_rate, .data$shock_year, .data$net_present_value_baseline,
        .data$net_present_value_shock, .data$net_present_value_difference,
        .data$term, .data$pd_baseline, .data$pd_shock, .data$pd_difference
      )
  }

  return(list(
    company_trajectories = results_list$company_trajectories,
    crispy_output = crispy_output
  ))
}
