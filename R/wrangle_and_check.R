#' Check financial data
#'
#' Applies sanity checks to financial data.
#'
#' @param financial_data A data set of `financial_data`.
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
#'   financial_data = fin_data
#' )
check_financial_data <- function(financial_data,
                                 interactive_mode = FALSE) {
  expected_columns <- c(
    "company_name", "company_id", "pd", "net_profit_margin",
    "debt_equity_ratio", "volatility"
  )

  validate_data_has_expected_cols(
    data = financial_data,
    expected_columns = expected_columns
  )


  # ADO 2493 - Use distinct_all to remove duplicates from remaining CUC columns
  # since financial data is always equal for these columns
  financial_data <- financial_data %>%
    dplyr::select(
      .data$company_name, .data$company_id, .data$pd, .data$net_profit_margin,
      .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::distinct_all()

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
    financial_data = financial_data
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

#' Wrangle nad check production input data
#'
#' @param data A tibble holding production data.
#' @param start_year Numeric. Start year of the analysis
#' @param time_horizon Numeric. Number of years of forward looking production data
#'
#' @return Returns data invisibly.
wrangle_and_check_production_data <- function(data, start_year, time_horizon) {
  data <- data %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      tidyr::nesting(!!!rlang::syms(prod_nesting_vars_lookup))
    ) %>%
    dplyr::mutate(plan_tech_prod = dplyr::if_else(is.na(.data$plan_tech_prod), 0, .data$plan_tech_prod))

  return(data)
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
      scenario_name, scenario_geography, id, company_name, ald_sector, technology, year
    ) %>%
    dplyr::group_by(
      scenario_name, scenario_geography, id, company_name, ald_sector, technology
    ) %>%
    # NOTE: this assumes emissions factors stay constant after forecast and prod not continued
    tidyr::fill(
      # TODO: what is company_id even doing here?
      # company_id,
      pd, net_profit_margin, debt_equity_ratio, volatility,
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
check_valid_financial_data_values <- function(financial_data) {
  if (any(financial_data$pd < 0 | financial_data$pd >= 1)) {
    stop("Implausibe value(s) < 0 or >= 1 for pd detected. Please check.")
  }

  if (any(financial_data$net_profit_margin <= 0 | financial_data$net_profit_margin > 1)) {
    stop("Implausibe value(s) <= 0 or > 1 for net_profit_margin detected. Please check.")
  }


  if (any(financial_data$debt_equity_ratio < 0)) {
    stop("Implausibe value(s) < 0 for debt_equity_ratio detected. Please check.")
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

  # company trajectories ----------------------------------------------------
  company_trajectories <- results_list$company_trajectories %>%
    dplyr::select(
      .data$scenario_name, .data$company_name, .data$year,
      .data$scenario_geography, .data$ald_sector, .data$technology,
      .data$plan_tech_prod, .data$phase_out, .data$baseline,
      .data$scen_to_follow_aligned, .data$late_sudden, .data$id,
      .data$pd, .data$net_profit_margin, .data$debt_equity_ratio,
      .data$volatility, .data$Baseline_price, .data$late_sudden_price,
      .data$net_profits_baseline, .data$net_profits_ls,
      .data$discounted_net_profit_baseline, .data$discounted_net_profit_ls,
      !!!rlang::syms(sensitivity_analysis_vars)
    ) %>%
    dplyr::rename(
      company_id = .data$id,
      production_plan_company_technology = .data$plan_tech_prod,
      # TODO: add once ADO3530 is merged
      # direction_of_target = .data$direction,
      production_baseline_scenario = .data$baseline,
      production_target_scenario = .data$scen_to_follow_aligned,
      production_shock_scenario = .data$late_sudden,
      price_baseline_scenario = .data$Baseline_price,
      price_shock_scenario = .data$late_sudden_price,
      net_profits_baseline_scenario = .data$net_profits_baseline,
      net_profits_shock_scenario = .data$net_profits_ls,
      discounted_net_profits_baseline_scenario = .data$discounted_net_profit_baseline,
      discounted_net_profits_shock_scenario = .data$discounted_net_profit_ls
    )

  # Crispy Results -----

  crispy_output <- results_list$company_technology_npv %>%
    dplyr::inner_join(
      results_list$company_pd_changes_overall,
      by = c(
        "scenario_name", "scenario_geography", "company_name", "ald_sector",
        "baseline_scenario_arg", "shock_scenario_arg", "lgd_arg",
        "risk_free_rate_arg", "discount_rate_arg", "growth_rate_arg",
        "div_netprofit_prop_coef_arg", "shock_year_arg"
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
    company_trajectories = company_trajectories,
    crispy_output = crispy_output
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
        "company_name", "year", "scenario_geography", "ald_sector", "technology",
        sensitivity_analysis_vars
      )
    ) %>%
    # not considering those two variables when checking for missings because
    # acceptable missing pattern is checked in ADO 4919
    dplyr::select(-.data$production_plan_company_technology) %>%
    report_missings(
      name_data = "Company Trajectories"
    )

  # crispy results ----------------------------------------------------
  wrangled_results_list$crispy_output %>%
    report_missings(
      name_data = "CRISPY Results"
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "company_name", "sector", "business_unit", "roll_up_type",
        "scenario_geography", "baseline_scenario", "shock_scenario", "lgd",
        "risk_free_rate", "discount_rate", "dividend_rate", "growth_rate",
        "shock_year", "term"
      )
    )

  return(invisible(wrangled_results_list))
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

add_term_to_trajectories <- function(annual_profits, production_data) {
  distinct_company_terms <- production_data %>%
    dplyr::select(company_name, term) %>%
    dplyr::distinct_all()

  report_duplicates(data = distinct_company_terms, cols = names(distinct_company_terms))

  annual_profits_with_term <- annual_profits %>%
    dplyr::inner_join(distinct_company_terms, by = c("company_name"))

  return(annual_profits_with_term)
}
