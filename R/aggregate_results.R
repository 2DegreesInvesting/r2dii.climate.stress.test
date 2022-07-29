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

  # Aggregate Market Risk results -----

  # validate value changes on company technology level -------------------------
  validate_data_has_expected_cols(
    data = results_list$company_technology_value_changes,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "VaR_tech_company", sensitivity_analysis_vars
    )
  )

  # join exposure on most granular value changes
  company_technology_value_changes <- results_list$company_technology_value_changes %>%
    join_exposure_on_value_changes(
      exposure = results_list$exposure_by_technology_and_company,
      assets_under_management = results_list$port_aum,
      sensitivity_analysis_vars = sensitivity_analysis_vars
    )

  # apply value changes to granular exposures in given portfolio
  company_technology_value_changes <- company_technology_value_changes %>%
    apply_value_change_to_exposure()

  # aggregate value changes to company level in given portfolio
  company_value_changes <- company_technology_value_changes %>%
    aggregate_value_change_to_company_level(iter_var = iter_var) %>%
    # aggregate value changes in portfolio to technology level
    aggregate_value_change_to_technology_level(iter_var = iter_var) %>%
    # aggregate value changes to sector level in given portfolio
    aggregate_value_change_to_sector_level(iter_var = iter_var) %>%
    # aggregate value changes to portfolio level and all covered sectors
    aggregate_value_change_to_analysed_sectors(iter_var = iter_var)

  company_value_changes <- company_value_changes %>%
    dplyr::select(-c(.data$plan_carsten, .data$plan_sec_carsten, .data$year))

  # Aggregate Credit Risk Results -----

  # prepare exposure data for credit risk results
  exposure_at_default_credit <- results_list$exposure_by_technology_and_company %>%
    prepare_exposure_for_credit_risk_format()

  company_pd_changes_annual <- results_list$company_pd_changes_annual %>%
    join_credit_exposure_on_pd_changes(exposure_at_default_credit)

  company_pd_changes_annual <- company_pd_changes_annual %>%
    aggregate_pd_change_to_sector_level(horizon = "annual", iter_var = iter_var)

  company_pd_changes_overall <- results_list$company_pd_changes_overall %>%
    join_credit_exposure_on_pd_changes(exposure_at_default_credit)

  company_pd_changes_overall <- company_pd_changes_overall %>%
    aggregate_pd_change_to_sector_level(horizon = "overall", iter_var = iter_var)

  # Aggregate Crispy Results -----

  crispy_output <- results_list$company_technology_npv %>%
    dplyr::inner_join(
      company_pd_changes_overall,
      by = c(
        "investor_name", "portfolio_name", "scenario_name", "scenario_geography",
        "company_name", "ald_sector", "asset_type_arg", "baseline_scenario_arg",
        "shock_scenario_arg", "lgd_arg", "risk_free_rate_arg", "discount_rate_arg",
        "growth_rate_arg", "div_netprofit_prop_coef_arg", "shock_year_arg",
        "fallback_term_arg", "use_company_terms_arg"
      )
    )

  crispy_output <- crispy_output %>%
    dplyr::mutate(
      roll_up_type = dplyr::if_else(
        .data$asset_type_arg == "bonds",
        "financial_control",
        "equity_ownership"
      )
    ) %>%
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
      .data$company_name, .data$id, .data$sector, .data$business_unit,
      .data$roll_up_type, .data$scenario_geography, .data$baseline_scenario,
      .data$shock_scenario, .data$lgd, .data$risk_free_rate, .data$discount_rate,
      .data$dividend_rate, .data$growth_rate, .data$shock_year,
      .data$net_present_value_baseline, .data$net_present_value_shock,
      .data$net_present_value_difference, .data$term, .data$pd_baseline,
      .data$pd_shock, .data$pd_difference
    )

  return(list(
    company_value_changes = company_value_changes,
    company_expected_loss = results_list$company_expected_loss,
    company_pd_changes_annual = company_pd_changes_annual,
    company_pd_changes_overall = company_pd_changes_overall,
    company_trajectories = results_list$company_trajectories,
    crispy_output = crispy_output
  ))
}

join_exposure_on_value_changes <- function(data,
                                           exposure,
                                           assets_under_management,
                                           sensitivity_analysis_vars) {
  data <- data %>%
    # Info re ADO 3384: this is where increasing technologies with zero start
    # value are removed (by now it should be zero production in t5)
    dplyr::inner_join(
      exposure,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "technology", "ald_sector", "scenario_geography", sensitivity_analysis_vars
      )
    )

  data <- data %>%
    dplyr::inner_join(
      assets_under_management,
      by = c("investor_name", "portfolio_name", sensitivity_analysis_vars)
    )

  return(data)
}

apply_value_change_to_exposure <- function(data) {
  data <- data %>%
    dplyr::mutate(
      tech_company_exposure = .data$asset_portfolio_value * .data$plan_carsten,
      tech_company_value_change = .data$tech_company_exposure * .data$VaR_tech_company / 100
    )

  return(data)
}

aggregate_value_change_to_company_level <- function(data, iter_var) {
  aggregation_vars <- c(
    "investor_name", "portfolio_name", "company_name", "scenario_geography"
  )

  if (iter_var != "standard") {
    aggregation_vars <- c(aggregation_vars, glue::glue("{iter_var}_arg"))
  }

  data <- data %>%
    dplyr::group_by(!!!rlang::syms(aggregation_vars)) %>%
    dplyr::mutate(
      VaR_company = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      company_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      company_value_change = .data$company_exposure * .data$VaR_company / 100
    ) %>%
    dplyr::ungroup()

  return(data)
}

aggregate_value_change_to_technology_level <- function(data, iter_var) {
  aggregation_vars <- c(
    "investor_name", "portfolio_name", "ald_sector", "technology",
    "scenario_geography"
  )

  if (iter_var != "standard") {
    aggregation_vars <- c(aggregation_vars, glue::glue("{iter_var}_arg"))
  }

  data <- data %>%
    dplyr::group_by(!!!rlang::syms(aggregation_vars)) %>%
    dplyr::mutate(
      VaR_technology = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_value_change = .data$technology_exposure * .data$VaR_technology / 100
    ) %>%
    dplyr::ungroup()

  return(data)
}

aggregate_value_change_to_sector_level <- function(data, iter_var) {
  aggregation_vars <- c(
    "investor_name", "portfolio_name", "ald_sector", "scenario_geography"
  )

  if (iter_var != "standard") {
    aggregation_vars <- c(aggregation_vars, glue::glue("{iter_var}_arg"))
  }

  data <- data %>%
    dplyr::group_by(!!!rlang::syms(aggregation_vars)) %>%
    dplyr::mutate(
      VaR_sector = sum(.data$VaR_technology * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_value_change = .data$sector_exposure * .data$VaR_sector / 100
    ) %>%
    dplyr::ungroup()

  return(data)
}

aggregate_value_change_to_analysed_sectors <- function(data,
                                                                     iter_var) {
  aggregation_vars <- c(
    "investor_name", "portfolio_name", "scenario_geography"
  )

  if (iter_var != "standard") {
    aggregation_vars <- c(aggregation_vars, glue::glue("{iter_var}_arg"))
  }

  data <- data %>%
    dplyr::group_by(!!!rlang::syms(aggregation_vars)) %>%
    dplyr::mutate(
      VaR_analysed_sectors = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_value_change = .data$analysed_sectors_exposure *
        .data$VaR_analysed_sectors / 100
    ) %>%
    dplyr::ungroup()

  return(data)
}

prepare_exposure_for_credit_risk_format <- function(data) {
  # distinct in order to remove unused technology level information, the
  # trivial year and the unneeded financial info
  data <- data %>%
    dplyr::distinct(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$ald_sector, .data$scenario_geography, .data$plan_sec_carsten
    )

  return(data)
}

join_credit_exposure_on_pd_changes <- function(data,
                                               exposure) {
  data <- data %>%
    dplyr::inner_join(
      exposure,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "scenario_geography", "ald_sector"
      )
    )

  return(data)
}

aggregate_pd_change_to_sector_level <- function(data,
                                                horizon = c("annual", "overall"),
                                                iter_var) {
  horizon_group <- if (horizon == "annual") {"year"} else {"term"}

  aggregation_vars <- c(
    "investor_name", "portfolio_name", "ald_sector", "scenario_geography",
    horizon_group
  )

  if (iter_var != "standard") {
    aggregation_vars <- c(aggregation_vars, glue::glue("{iter_var}_arg"))
  }


  data <- data %>%
    dplyr::group_by(!!!rlang::syms(aggregation_vars)) %>%
    dplyr::mutate(
      PD_baseline_sector = stats::weighted.mean(
        .data$PD_baseline,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_late_sudden_sector = stats::weighted.mean(
        .data$PD_late_sudden,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_change_sector = stats::weighted.mean(
        .data$PD_change,
        w = .data$plan_sec_carsten, na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}

