#' Run stress testing for loans
#'
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `lgd_senior_claims_range_lookup`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `lgd_subordinated_claims_range_lookup`.
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame. For accepted range compare `terminal_value_range_lookup`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `risk_free_rate_range_lookup`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `discount_rate_range_lookup`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `div_netprofit_prop_coef_range_lookup`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `shock_year_range_lookup`.
#' @param term Numeric. A coefficient that determines for which maturity the
#'   expected loss should be calculated in the credit risk section. For accepted
#'   range compare `term_range_lookup`.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded.
#' @param credit_type Type of credit. For accepted values please compare
#'   `credit_type_loans`.
#' @return NULL
#' @export
run_stress_test_loans <- function(lgd_senior_claims = 0.45,
                                  lgd_subordinated_claims = 0.75,
                                  terminal_value = 0,
                                  risk_free_rate = 0.02,
                                  discount_rate = 0.02,
                                  div_netprofit_prop_coef = 1,
                                  shock_year = 2030,
                                  term = 2,
                                  company_exclusion = TRUE,
                                  credit_type = "credit_limit") {
  validate_input_values(
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    terminal_value = terminal_value,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    term = term,
    company_exclusion = company_exclusion,
    credit_type = credit_type
  )

  scenario_to_follow_baseline <- baseline_scenario_lookup
  scenario_to_follow_ls <- shock_scenario_lookup
  calculation_level <- calculation_level_lookup
  end_year <- end_year_lookup

  ###########################################################################
  # Project Initialisation---------------------------------------------------
  ###########################################################################

  # FIXME: Very bad solution for temporart use only
  source_all(c("stress_test_model_functions.R", "0_global_functions_st.R"))

  #### Project location----------------------------------------
  cfg_st <- config::get(file = "st_project_settings.yml")
  check_valid_cfg(cfg = cfg_st, expected_no_args = 5)
  project_name <- cfg_st$project_name

  #### Analysis Parameters----------------------------------------
  # Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

  # TODO: where to get this parameter
  cfg <- config::get(file = file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "AnalysisParameters.yml"))
  # OPEN: check_valid_cfg() not applicable here
  start_year <- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <- cfg$AnalysisPeriod$Years.Horizon

  ##### Filters----------------------------------------
  # The filter settings should comply with the filters from the parent PACTA project as per default
  # There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
  # move to config once mechanism to include/exclude filters from original pacta project exists

  scenario_geography_filter <- "Global"

  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls
    )
  )

  ###########################################################################
  # Load input datasets------------------------------------------------------
  ###########################################################################

  # Load PACTA results / loans portfolio------------------------
  # TODO: select the right scenarios
  # TODO: select the right geography
  # TODO: must contain term and initial PD
  loanbook_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", paste0("company_results_lb_", project_name, ".csv"))

  pacta_loanbook_results <- read_pacta_results(
    path = loanbook_path,
    asset_type = "loans",
    level = calculation_level
  ) %>%
    format_loanbook_st(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder,
      credit = paste0("loan_share_", credit_type)
    ) %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon,
      scenario_geography_filter = scenario_geography_filter,
      scenarios_filter = scenarios_filter,
      equity_market_filter = cfg$Lists$Equity.Market.List
    ) %>%
    # ADO 1943 - for the time being, one global term value is set by the user.
    # TODO: next version to allow term input on holding/company level
    dplyr::mutate(term = term)

  sector_credit_type <- paste0("sector_loan_size_", credit_type)
  credit_currency <- paste0("loan_size_", credit_type, "_currency")

  sector_exposures <- readr::read_csv(
    file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", paste0("portfolio_overview_", project_name, ".csv")),
    col_types = "cddcddc"
  ) %>%
    dplyr::mutate(
      sector_ald = dplyr::case_when(
        sector_ald == "power" ~ "Power",
        sector_ald == "oil and gas" ~ "Oil&Gas",
        sector_ald == "coal" ~ "Coal",
        sector_ald == "automotive" ~ "Automotive",
        sector_ald == "steel" ~ "Steel",
        sector_ald == "cement" ~ "Cement",
        TRUE ~ sector_ald
      )
    ) %>%
    dplyr::select(
      sector_ald,
      !!rlang::sym(sector_credit_type),
      !!rlang::sym(credit_currency)
    ) %>%
    dplyr::rename(
      financial_sector = sector_ald,
      valid_value_usd = !!rlang::sym(sector_credit_type)
    ) %>%
    dplyr::mutate(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder
    )
  # TODO: potentially convert currencies to USD or at least common currency

  # Load transition scenarios that will be run by the model
  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year,
    shock_years = shock_year
  )

  # Load project agnostic data sets -----------------------------------------
  input_data_list <- read_and_prepare_project_agnostic_data(
    start_year = start_year,
    end_year = end_year,
    company_exclusion = company_exclusion,
    scenario_geography_filter = scenario_geography_filter,
    asset_type = "loans"
  )

  excluded_companies <- input_data_list$excluded_companies
  scenario_data <- input_data_list$scenario_data
  financial_data_loans <- input_data_list$financial_data %>%
    dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name))

  # check scenario availability across data inputs for bonds
  check_scenario_availability(
    portfolio = pacta_loanbook_results,
    scen_data = scenario_data,
    scenarios = scenarios_filter
  )

  # Prepare sector exposure data-------------------------------------------------
  # ...for loans portfolio-------------------------------------------------------
  # TODO: validate
  loan_book_port_aum <- calculate_aum(sector_exposures)

  ## if we use the integral/overshoot late&sudden method, and we use company production plans the first 5 years
  ## the integral method works on company level, however,
  ## when we aggregate the company LS trajectories to port-technology level, the integrals of SDS and LS are not the same, due to 2 reasons:
  ## 1) for companies that outperform SDS, capacity shhould not be compensated for, hence we take a LS trajecorty that equal SDS
  ## 2) there are cases for which the linear compensation is so strong, that the LS production falls below zero, which is then set to zero (as negative production is not possible), hence we have an underestimation in overshoot
  ## For these two reasons, if we use company production plans, we perform the integral method on technology level (and not on company level), until we had a proper session on how to deal with these issues

  ###########################################################################
  # Calculation of results---------------------------------------------------
  ###########################################################################
  df_prices <- input_data_list$df_price %>%
    calc_late_sudden_prices(
      baseline_scenario = scenario_to_follow_baseline,
      transition_scenario = transition_scenario,
      start_year = start_year
    )

  loanbook_annual_profits <- pacta_loanbook_results %>%
    convert_power_cap_to_generation(
      capacity_factors_power = input_data_list$capacity_factors_power,
      baseline_scenario = scenario_to_follow_baseline
    ) %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
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
    )

  if (company_exclusion) {
    loanbook_annual_profits <- loanbook_annual_profits %>%
      exclude_companies(
        exclusion = excluded_companies,
        scenario_baseline = scenario_to_follow_baseline,
        scenario_ls = scenario_to_follow_ls
      )
  }

  rows_loanbook <- nrow(loanbook_annual_profits)
  loanbook_annual_profits <- loanbook_annual_profits %>%
    # ADO 879: removed company id from join, but should be re-introduced later on
    dplyr::inner_join(
      financial_data_loans,
      by = c("company_name", "ald_sector", "technology")
    )
  cat(
    "number of rows dropped from loan book by joining financial data on
      company_name, ald_sector and technology = ",
    rows_loanbook - nrow(loanbook_annual_profits), "\n"
  )
  # TODO: ADO 879 - note which companies are removed here, due to mismatch of
  # sector/tech in the financial data and the portfolio

  loanbook_annual_profits <- loanbook_annual_profits %>%
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

  loanbook_annual_profits <- loanbook_annual_profits %>%
    join_price_data(df_prices = df_prices) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate)
  # TODO: ADO 879 - note rows with zero profits/NPVs will produce NaN in the Merton model

  qa_annual_profits_lbk <- loanbook_annual_profits %>%
    dplyr::mutate(year_of_shock = transition_scenario$year_of_shock)

  plan_carsten_loanbook <- pacta_loanbook_results %>%
    dplyr::filter(
      .data$year == .env$start_year,
      .data$technology %in% technologies_lookup,
      .data$scenario_geography == .env$scenario_geography_filter,
      .data$scenario %in% .env$scenario_to_follow_ls
    )

  financial_data_loans_pd <- financial_data_loans %>%
    dplyr::select(company_name, company_id, ald_sector, technology, pd)

  report_duplicates(
    data = financial_data_loans_pd,
    cols = names(financial_data_loans_pd)
  )

  rows_plan_carsten <- nrow(plan_carsten_loanbook)
  plan_carsten_loanbook <- plan_carsten_loanbook %>%
    # ADO 879: removed company id from join, but should be re-introduced later on
    dplyr::inner_join(
      financial_data_loans_pd,
      by = c("company_name", "ald_sector", "technology")
    )
  cat(
    "number of rows dropped from technology_exposure by joining financial data
      on company_name, ald_sector and technology = ",
    rows_plan_carsten - nrow(plan_carsten_loanbook), "\n"
  )
  # TODO: ADO 879 - note which companies are removed here, due to mismatch of
  # sector/tech in the financial data and the portfolio
  # TODO: what to do with entries that have NAs for pd?

  plan_carsten_loanbook <- plan_carsten_loanbook %>%
    dplyr::select(
      investor_name, portfolio_name, company_name, ald_sector, technology,
      scenario_geography, year, plan_carsten, plan_sec_carsten, term, pd
    )

  report_duplicates(
    data = plan_carsten_loanbook,
    cols = names(plan_carsten_loanbook)
  )

  loanbook_results <- company_asset_value_at_risk(
      data = loanbook_annual_profits,
      terminal_value = terminal_value,
      shock_scenario = transition_scenario,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      plan_carsten = plan_carsten_loanbook,
      port_aum = loan_book_port_aum,
      flat_multiplier = 0.15,
      exclusion = excluded_companies
    )

  loanbook_overall_pd_changes <- loanbook_annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  loanbook_expected_loss <- company_expected_loss(
    data = loanbook_overall_pd_changes,
    loss_given_default = lgd_senior_claims,
    exposure_at_default = plan_carsten_loanbook,
    # TODO: what to do with this? some sector level exposure for loanbook?
    port_aum = loan_book_port_aum
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient output from overall pd changes or related financial data inputs

  loanbook_annual_pd_changes <- calculate_pd_change_annual(
    data = loanbook_annual_profits,
    shock_year = transition_scenario$year_of_shock,
    end_of_analysis = end_year,
    risk_free_interest_rate = risk_free_rate
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)


  write_stress_test_results(results = loanbook_results,
                            expected_loss = loanbook_expected_loss,
                            annual_pd_changes = loanbook_annual_pd_changes,
                            asset_type = "loans",
                            calculation_level = calculation_level)
}
