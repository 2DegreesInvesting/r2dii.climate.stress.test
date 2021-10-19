#' Run stress testing for equity
#'
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `lgd_senior_claims_range_lookup`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `lgd_subordinated_claims_range_lookup`.
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame. For accepted range compare `terminal_value_lookup`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `risk_free_rate_lookup`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `discount_rate_lookup`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `div_netprofit_prop_coef_range_lookup`.
#' @param company_exlusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded.
#' @return NULL
#' @export
run_stress_test_equity <- function(lgd_senior_claims = 0.45,
                                   lgd_subordinated_claims = 0.75,
                                   terminal_value = 0,
                                   risk_free_rate = 0.02,
                                   discount_rate = 0.02,
                                   div_netprofit_prop_coef = 1,
                                   company_exclusion = TRUE) {
  validate_input_values(
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    terminal_value = terminal_value,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    company_exclusion = company_exclusion
  )

  scenario_to_follow_baseline <- baseline_scenario_lookup
  scenario_to_follow_ls <- shock_scenario_lookup
  calculation_level <- calculation_level_lookup

  ###########################################################################
  # Project Initialisation---------------------------------------------------
  ###########################################################################

  # FIXME: Very bad solution for temporart use only
  source_all(c("stress_test_model_functions.R", "0_global_functions_st.R"))

  cfg_st <- config::get(file = "st_project_settings.yml")
  check_valid_cfg(cfg = cfg_st, expected_no_args = 5)
  project_name <- cfg_st$project_name
  twodii_internal <- cfg_st$project_internal$twodii_internal
  project_location_ext <- cfg_st$project_internal$project_location_ext
  price_data_version <- cfg_st$price_data_version


  data_location <- file.path(get_st_data_path(), data_path())

  # Analysis Parameters----------------------------------------
  # Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

  cfg <- config::get(file = file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "AnalysisParameters.yml"))
  # OPEN: check_valid_cfg() not applicable here
  start_year <- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <- cfg$AnalysisPeriod$Years.Horizon

  # Filters----------------------------------------
  # The filter settings should comply with the filters from the parent PACTA project as per default
  # There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
  # move to config once mechanism to include/exclude filters from original pacta project exists
  scenario_geography_filter <- "Global"

  # Model variables----------------------------------------
  #### OPEN: This should be moved into a StressTestModelParameters.yml
  cfg_mod <- config::get(file = "model_parameters.yml")

  # OPEN: wrap reading in of params in function and move to global_functions
  end_year <- cfg_mod$end_year # Set to 2040 cause current scenario data goes until 2040. can be extended when WEO2020 turns out extended horizon

  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls
    )
  )

  ###########################################################################
  # Load input datasets------------------------------------------------------
  ###########################################################################
  financial_data_equity <- read_company_data(
    path = create_stressdata_masterdata_file_paths()$listed_equity,
    asset_type = "equity"
  ) %>%
    wrangle_financial_data(start_year = start_year)

  # Load PACTA results / equity portfolio------------------------
  equity_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", paste0("Equity_results_", calculation_level, ".rda"))

  pacta_equity_results <- read_pacta_results(
    path = equity_path,
    asset_type = "equity",
    level = calculation_level
  ) %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon,
      scenario_geography_filter = scenario_geography_filter,
      scenarios_filter = scenarios_filter,
      equity_market_filter = cfg$Lists$Equity.Market.List
    ) %>%
    dplyr::group_by(company_name) %>%
    dplyr::mutate(
      term = round(runif(n = 1, min = 1, max = 10), 0) # TODO: temporary addition, needs to come directly from input
    ) %>%
    dplyr::ungroup()

  # Load sector exposures of portfolio------------------------
  sector_exposures <- readRDS(file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "overview_portfolio.rda")) %>%
    wrangle_and_check_sector_exposures_eq_cb(asset_type = "Equity")

  # Load policy shock transition scenarios--------------------
  transition_scenarios <- read_transition_scenarios(
    path = file.path(data_location, "transition_scenario_input.csv"),
    start_of_analysis = start_year,
    end_of_analysis = end_year
  )

  # Load utilization factors power----------------------------
  capacity_factors_power <- read_capacity_factors(
    path = file.path(data_location, "capacity_factors_WEO_2020.csv"),
    version = "new"
  )

  # Load scenario data----------------------------------------
  scenario_data <- read_scenario_data(
    path = file.path(data_location, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
  ) %>%
    wrangle_scenario_data(start_year = start_year, end_year = end_year) %>%
    dplyr::filter(
      .data$ald_sector %in% sectors_lookup &
        .data$technology %in% technologies_lookup &
        .data$scenario_geography == scenario_geography_filter
    )

  # Load price data----------------------------------------
  df_price <- read_price_data(
    path = file.path(data_location, paste0("prices_data_", price_data_version, ".csv")),
    version = "old",
    expected_technologies = technologies_lookup
  ) %>%
    dplyr::filter(year >= start_year) %>%
    check_price_consistency(start_year = start_year)

  # Load excluded companies-------------------------------
  if (company_exclusion) {
    excluded_companies <- readr::read_csv(
      file.path(data_location, "exclude-companies.csv"),
      col_types = "cc"
    )
  } else {
    excluded_companies <- NULL
  }

  # check scenario availability across data inputs for equity
  check_scenario_availability(
    portfolio = pacta_equity_results,
    scen_data = scenario_data,
    scenarios = scenarios_filter
  )

  # Prepare sector exposure data-------------------------------------------------
  # ...for equity portfolio------------------------------------------------------
  equity_port_aum <- calculate_aum(sector_exposures)

  #### OPEN: both objects in condition not available as of now,
  # since they are read in into a loop afterwards
  # deactivated, for the time being

  # if(use_prod_forecasts_ls & overshoot_method){
  ## i.e. we use the integral/overshoot late&sudden method, and we use company production plans the first 5 years
  ## the integral method works on company level, however,
  ## when we aggregate the company LS trajectories to port-technology level, the integrals of SDS and LS are not the same, due to 2 reasons:
  ## 1) for companies that outperform SDS, capacity shhould not be compensated for, hence we take a LS trajecorty that equal SDS
  ## 2) there are cases for which the linear compensation is so strong, that the LS production falls below zero, which is then set to zero (as negative production is not possible), hence we have an underestimation in overshoot
  ## For these two reasons, if we use company production plans, we perform the integral method on technology level (and not on company level), until we had a proper session on how to deal with these issues

  ###########################################################################
  # Calculation of results---------------------------------------------------
  ###########################################################################

  # Equity results  -------------------------------------------------------------

  equity_results <- c()
  qa_annual_profits_eq <- c()
  equity_expected_loss <- c()
  equity_annual_pd_changes <- c()
  qa_pd_changes <- c()

  for (i in seq(1, nrow(transition_scenarios))) {
    transition_scenario_i <- transition_scenarios[i, ]
    overshoot_method <- transition_scenario_i$overshoot_method
    year_of_shock <- transition_scenario_i$year_of_shock
    duration_of_shock <- transition_scenario_i$duration_of_shock
    use_prod_forecasts_baseline <- transition_scenario_i$use_prod_forecasts_baseline
    use_prod_forecasts_ls <- transition_scenario_i$use_prod_forecasts_ls

    # Create shock scenario dataframe for scenario i
    # For now we use the old shock scenario dataframe format. Should change this over time as its far from optimal
    shock_scenario <- create_shock_scenario(transition_scenario = transition_scenario_i)

    # Calculate late and sudden prices for scenario i
    df_prices <- df_price %>%
      dplyr::mutate(Baseline = !!rlang::sym(scenario_to_follow_baseline)) %>%
      dplyr::rename(
        year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
        SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
      ) %>%
      dplyr::group_by(ald_sector, technology) %>%
      #### OPEN: Potentially a problem with the LS price calculation. Concerning warning
      dplyr::mutate(
        late_sudden_price = late_sudden_prices(
          SDS_price = SDS_price,
          Baseline_price = Baseline_price,
          overshoot_method = overshoot_method,
          year_of_shock = year_of_shock,
          start_year = start_year,
          duration_of_shock = duration_of_shock
        )
      ) %>%
      dplyr::ungroup()

    # Convert capacity (MW) to generation (MWh) for power sector
    equity_annual_profits <- pacta_equity_results %>%
      convert_power_cap_to_generation(
        capacity_factors_power = capacity_factors_power,
        baseline_scenario = scenario_to_follow_baseline
      ) %>%
      extend_scenario_trajectory(
        scenario_data = scenario_data,
        start_analysis = start_year,
        end_analysis = end_year,
        time_frame = time_horizon
      ) %>%
      set_baseline_trajectory(
        scenario_to_follow_baseline = scenario_to_follow_baseline,
        use_prod_forecasts = use_prod_forecasts_baseline
      ) %>%
      set_ls_trajectory(
        scenario_to_follow_ls = scenario_to_follow_ls,
        shock_scenario = shock_scenario,
        use_production_forecasts_ls = use_prod_forecasts_ls,
        overshoot_method = overshoot_method,
        scenario_to_follow_ls_aligned = scenario_to_follow_ls,
        start_year = start_year,
        end_year = end_year,
        analysis_time_frame = time_horizon
      )

    if (company_exclusion) {
      equity_annual_profits <- equity_annual_profits %>%
        exclude_companies(
          exclusion = excluded_companies,
          scenario_baseline = scenario_to_follow_baseline,
          scenario_ls = scenario_to_follow_ls
        )
    }

    rows_equity <- nrow(equity_annual_profits)

    equity_annual_profits <- equity_annual_profits %>%
      dplyr::inner_join(
        financial_data_equity,
        by = c("company_name", "ald_sector", "technology")
      )

    cat(
      "number of rows dropped by joining financial data on
      company_name, ald_sector and technology = ",
      rows_equity - nrow(equity_annual_profits), "\n"
    )
    # TODO: ADO 879 - note which companies are removed here, due to mismatch

    equity_annual_profits <- equity_annual_profits %>%
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

    equity_annual_profits <- equity_annual_profits %>%
      join_price_data(df_prices = df_prices) %>%
      calculate_net_profits() %>%
      dcf_model_techlevel(discount_rate = discount_rate)

    qa_annual_profits_eq <- qa_annual_profits_eq %>%
      dplyr::bind_rows(
        equity_annual_profits %>%
          dplyr::mutate(year_of_shock = transition_scenario_i$year_of_shock)
      )

    plan_carsten_equity <- pacta_equity_results %>%
      dplyr::filter(
        .data$year == start_year,
        .data$technology %in% technologies_lookup,
        .data$scenario_geography == scenario_geography_filter,
        .data$scenario %in% .env$scenario_to_follow_ls
      )

    financial_data_equity_pd <- financial_data_equity %>%
      dplyr::select(company_name, ald_sector, technology, pd)

    report_duplicates(
      data = financial_data_equity_pd,
      cols = names(financial_data_equity_pd)
    )

    rows_plan_carsten <- nrow(plan_carsten_equity)

    plan_carsten_equity <- plan_carsten_equity %>%
      dplyr::inner_join(financial_data_equity_pd, by = c("company_name", "ald_sector", "technology"))

    cat(
      "number of rows dropped from technology_exposure by joining financial data
      on company_name, ald_sector and technology = ",
      rows_plan_carsten - nrow(plan_carsten_equity), "\n"
    )
    # TODO: ADO 879 - note which companies are removed here, due to mismatch

    equity_annual_profits <- equity_annual_profits %>%
      dplyr::filter(!is.na(company_id))

    plan_carsten_equity <- plan_carsten_equity %>%
      dplyr::select(
        investor_name, portfolio_name, company_name, ald_sector, technology,
        scenario_geography, year, plan_carsten, plan_sec_carsten, term, pd
      )

    report_duplicates(
      data = plan_carsten_equity,
      cols = names(plan_carsten_equity)
    )

    equity_results <- dplyr::bind_rows(
      equity_results,
      company_asset_value_at_risk(
        data = equity_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_equity,
        port_aum = equity_port_aum,
        flat_multiplier = 1,
        exclusion = excluded_companies
      )
    )

    equity_overall_pd_changes <- equity_annual_profits %>%
      calculate_pd_change_overall(
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)

    equity_expected_loss <- dplyr::bind_rows(
      equity_expected_loss,
      company_expected_loss(
        data = equity_overall_pd_changes,
        loss_given_default = lgd_subordinated_claims, # TODO: which one?
        exposure_at_default = plan_carsten_equity,
        port_aum = equity_port_aum
      )
    )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient output from overall pd changes or related financial data inputs

    equity_annual_pd_changes <- dplyr::bind_rows(
      equity_annual_pd_changes,
      calculate_pd_change_annual(
        data = equity_annual_profits,
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )
    )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)
  }

  write_stress_test_results(
    results = equity_results,
    expected_loss = equity_expected_loss,
    annual_pd_changes = equity_annual_pd_changes,
    asset_type = "equity",
    calculation_level = calculation_level
  )
}
