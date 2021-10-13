run_stress_test_loans <- function() {
  ###########################################################################
  # Project Initialisation---------------------------------------------------
  ###########################################################################

  # FIXME: Very bad solution for temporart use only
  source_all(c("stress_test_model_functions.R", "0_global_functions_st.R"))

  #### Project location----------------------------------------

  # Set Project Settings

  # within the "st_project_setting.yml" config file, set the project_name, the twodii_internal switch,
  # and the external data locations, if necessary.
  # the project_name will determine the name of the folder that is to be used for locating
  # input and output directories for this project
  # Set twodii_internal to TRUE to run the analysis on an internal 2dii laptop
  # This setting uses the dropbox connection for data import
  # Set twodii_internal to FALSE, tu use external data locations
  # Specify these data locations in the config file "st_project_settings.yml" in the repo


  cfg_st <- config::get(file = "st_project_settings.yml")
  check_valid_cfg(cfg = cfg_st, expected_no_args = 5)
  project_name <- cfg_st$project_name
  twodii_internal <- cfg_st$project_internal$twodii_internal
  project_location_ext <- cfg_st$project_internal$project_location_ext
  price_data_version <- cfg_st$price_data_version
  calculation_level <- "company"
  company_exclusion <- cfg_st$company_exclusion

  data_location <- file.path(get_st_data_path(), data_path())

  # set input path
  # TODO: barely needed project_path and results_path need to be set
  set_project_paths(
    project_name = project_name,
    twodii_internal = twodii_internal,
    project_location_ext = project_location_ext
  )

  #### Analysis Parameters----------------------------------------
  # Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

  # TODO: where to get this parameter
  cfg <- config::get(file = file.path(project_location, "10_Parameter_File", "AnalysisParameters.yml"))
  # OPEN: check_valid_cfg() not applicable here
  start_year <- 2020
  time_horizon <- cfg$AnalysisPeriod$Years.Horizon

  ##### Filters----------------------------------------
  # The filter settings should comply with the filters from the parent PACTA project as per default
  # There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
  # move to config once mechanism to include/exclude filters from original pacta project exists

  scenario_geography_filter <- "Global"

  #### Model variables----------------------------------------
  #### OPEN: This should be moved into a StressTestModelParameters.yml
  cfg_mod <- config::get(file = "model_parameters.yml")

  # OPEN: wrap reading in of params in function and move to global_functions
  end_year <- cfg_mod$end_year # Set to 2040 cause current scenario data goes until 2040. can be extended when WEO2020 turns out extended horizon

  # Scenarios in the model_parameters.yml file must have the short names (SDS, NPS, etc)
  scenario_to_follow_baseline <- cfg_mod$scenarios$scenario_to_follow_baseline # sets which scenario trajectory the baseline scenario follows
  scenario_to_follow_ls <- cfg_mod$scenarios$scenario_to_follow_ls # sets which scenario trajectory LS scenario follows after shock period
  scenario_to_follow_ls_aligned <- cfg_mod$scenarios$scenario_to_follow_ls_aligned

  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls,
      scenario_to_follow_ls_aligned
    )
  )

  # Moved to transition scenario input file:
  # use_prod_forecasts_baseline <- FALSE   # TRUE: Use company production forecasts until no longer available for baseline. FALSE: Let baseline immediately follow scenario (and not follow production forecasts first)
  # use_prod_forecasts_ls <- FALSE  # TRUE: Use company production forecasts until no longer available for late&sudden. FALSE: Let late&sudden scenario immediately follow IEA scenario (and not follow production forecasts first)
  # overshoot_method <- TRUE          # TRUE: use integral/overshoot method for late&sudden trajectory, FALSE: use user defined shocks
  ##### OPEN: this is currently not used, defined in transition_scenario loop
  # duration_div <- duration_of_shock

  discount_rate <- cfg_mod$financials$discount_rate # Discount rate
  ##### OPEN: this needs to be estimated based on data
  terminal_value <- cfg_mod$financials$terminal_value
  div_netprofit_prop_coef <- cfg_mod$financials$div_netprofit_prop_coef # determine this value using bloomberg data
  risk_free_rate <- cfg_mod$financials$risk_free_rate
  lgd_senior_claims <- cfg_mod$financials$lgd_senior_claims
  lgd_subordinated_claims <- cfg_mod$financials$lgd_subordinated_claims

  # TODO: move to config file
  credit_type <- c(
    # "outstanding"
    "credit_limit"
  )
  loan_share_credit_type <- paste0("loan_share_", credit_type)

  ###########################################################################
  # Load input datasets------------------------------------------------------
  ###########################################################################

  # Load company financial and production data-----------------------------------
  # ... get file paths for stresstest masterdata --------------------------------
  stresstest_masterdata_files <- create_stressdata_masterdata_file_paths(
    data_prep_timestamp = cfg$TimeStamps$DataPrep.Timestamp,
    twodii_internal = twodii_internal
  )

  # ... for loans----------------------------------------------------------------
  financial_data_loans <- read_company_data(
    path = stresstest_masterdata_files$loans,
    asset_type = "loans"
  )


  # Load PACTA results / loans portfolio------------------------
  # TODO: select the right scenarios
  # TODO: select the right geography
  # TODO: must contain term and initial PD
  loanbook_path <- path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "40_Results", paste0("company_results_lb_", project_name, ".csv"))

  pacta_loanbook_results <- read_pacta_results(
    path = loanbook_path,
    asset_type = "loans",
    level = calculation_level
  ) %>%
    format_loanbook_st(
      investor_name = investor_name_placeholder,
      portfolio_name = investor_name_placeholder,
      credit = loan_share_credit_type
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

  sector_credit_type <- paste0("sector_loan_size_", credit_type)
  credit_currency <- paste0("loan_size_", credit_type, "_currency")

  sector_exposures <- read_csv(
    path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs", paste0("portfolio_overview_", project_name, ".csv")),
    col_types = "cddcddc"
  ) %>%
    dplyr::mutate(
      sector_ald = case_when(
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
  transition_scenarios <- read_transition_scenarios(
    path = file.path(data_location, "transition_scenario_input.csv"),
    start_of_analysis = start_year,
    end_of_analysis = end_year
  )

  # Load utilization factors power
  capacity_factors_power <- read_capacity_factors(
    path = file.path(data_location, "capacity_factors_WEO_2020.csv"),
    version = "new"
  )

  # Load scenario data----------------------------------------
  scen_data_file <- ifelse(twodii_internal == TRUE,
                           path_dropbox_2dii("PortCheck", "00_Data", "01_ProcessedData", "03_ScenarioData", paste0("Scenarios_AnalysisInput_", start_year, ".csv")),
                           file.path(data_location, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
  )

  # TODO: EITHER wrap check into more evocative function OR remove this when common format is agreed upon
  if (twodii_internal == TRUE | start_year < 2020) {
    scenario_data <- readr::read_csv(scen_data_file, col_types = "ccccccccnnnncnnn") %>%
      dplyr::filter(Indicator %in% c("Capacity", "Production", "Sales")) %>%
      dplyr::filter(!(Technology == "RenewablesCap" & !is.na(Sub_Technology))) %>%
      dplyr::select(-c(Sub_Technology, Indicator, AnnualvalIEAtech, refvalIEAtech, refvalIEAsec, mktFSRatio, techFSRatio)) %>%
      dplyr::rename(
        source = Source,
        scenario_geography = ScenarioGeography,
        scenario = Scenario,
        ald_sector = Sector,
        units = Units,
        technology = Technology,
        year = Year,
        direction = Direction,
        fair_share_perc = FairSharePerc
      ) %>%
      dplyr::mutate(scenario = stringr::str_replace(scenario, "NPSRTS", "NPS"))
  } else {
    scenario_data <- readr::read_csv(scen_data_file, col_types = "ccccccncn") %>%
      dplyr::rename(source = scenario_source)
  }

  scenario_data <- scenario_data %>%
    dplyr::filter(source %in% c("ETP2017", "WEO2019")) %>%
    # TODO: this should be set elsewhere
    dplyr::filter(!(source == "ETP2017" & ald_sector == "Power")) %>%
    dplyr::mutate(scenario = ifelse(stringr::str_detect(scenario, "_"), stringr::str_extract(scenario, "[^_]*$"), scenario)) %>%
    check_scenario_timeframe(start_year = start_year, end_year = end_year)

  # Correct for automotive scenario data error. CHECK IF ALREADY RESOLVED IN THE SCENARIO DATA, IF SO, DONT USE FUNCTION BELOW!
  scenario_data <- scenario_data %>%
    correct_automotive_scendata(interpolation_years = c(2031:2034, 2036:2039)) %>%
    dplyr::filter(
      ald_sector %in% sectors_lookup &
        technology %in% technologies_lookup &
        scenario_geography == scenario_geography_filter
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

  ###########################################################################
  # Data wrangling / preparation---------------------------------------------
  ###########################################################################

  # Prepare net profit margins loans----------------------

  financial_data_loans <- financial_data_loans %>%
    dplyr::mutate(net_profit_margin = profit_margin_preferred) %>%
    # TODO: logic unclear thus far
    dplyr::mutate(
      net_profit_margin = dplyr::case_when(
        net_profit_margin < 0 & dplyr::between(profit_margin_unpreferred, 0, 1) ~ profit_margin_unpreferred,
        net_profit_margin < 0 & profit_margin_unpreferred < 0 ~ 0,
        net_profit_margin < 0 & profit_margin_unpreferred > 1 ~ 0,
        net_profit_margin > 1 & dplyr::between(profit_margin_unpreferred, 0, 1) ~ profit_margin_unpreferred,
        net_profit_margin > 1 & profit_margin_unpreferred > 1 ~ 1,
        net_profit_margin > 1 & profit_margin_unpreferred < 0 ~ 1,
        TRUE ~ net_profit_margin
      )
    ) %>%
    dplyr::select(-c(profit_margin_preferred, profit_margin_unpreferred)) %>%
    dplyr::rename(
      debt_equity_ratio = leverage_s_avg,
      volatility = asset_volatility_s_avg
    ) %>%
    dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name)) %>%
    # ADO 879 - remove year and production/EFs to simplify joins that do not need yearly variation yet
    dplyr::filter(.data$year == .env$start_year) %>%
    dplyr::select(
      -c(
        .data$year, .data$ald_production_unit, .data$ald_production,
        .data$ald_emissions_factor_unit, .data$ald_emissions_factor
      )
    )
  # TODO: any logic/bounds needed for debt/equity ratio and volatility?

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

  # Bank loan book results (flat multiplier PRA 0.15) ---------------------------------------------------------

  loanbook_results <- c()
  qa_annual_profits_lbk <- c()
  loanbook_expected_loss <- c()
  loanbook_annual_pd_changes <- c()
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
    print(overshoot_method)
    # Calculate late and sudden prices for scenario i
    df_prices <- df_price %>%
      dplyr::mutate(Baseline = NPS) %>%
      # FIXME this should be parameterized!!
      dplyr::rename(
        year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
        SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
      ) %>%
      dplyr::group_by(ald_sector, technology) %>%
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

    loanbook_annual_profits <- pacta_loanbook_results %>%
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
        scenario_to_follow_ls_aligned = scenario_to_follow_ls_aligned,
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
      arrange(
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

    qa_annual_profits_lbk <- qa_annual_profits_lbk %>%
      dplyr::bind_rows(
        loanbook_annual_profits %>%
          dplyr::mutate(year_of_shock = transition_scenario_i$year_of_shock)
      )

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

    loanbook_results <- dplyr::bind_rows(
      loanbook_results,
      company_asset_value_at_risk(
        data = loanbook_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_loanbook,
        port_aum = loan_book_port_aum,
        flat_multiplier = 0.15,
        exclusion = excluded_companies
      )
    )

    loanbook_overall_pd_changes <- loanbook_annual_profits %>%
      calculate_pd_change_overall(
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)

    loanbook_expected_loss <- dplyr::bind_rows(
      loanbook_expected_loss,
      company_expected_loss(
        data = loanbook_overall_pd_changes,
        loss_given_default = lgd_senior_claims,
        exposure_at_default = plan_carsten_loanbook,
        # TODO: what to do with this? some sector level exposure for loanbook?
        port_aum = loan_book_port_aum
      )
    )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient output from overall pd changes or related financial data inputs

    loanbook_annual_pd_changes <- dplyr::bind_rows(
      loanbook_annual_pd_changes,
      calculate_pd_change_annual(
        data = loanbook_annual_profits,
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )
    )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)
  }

  # Output corporate loan book results
  loanbook_results %>% write_results(
    path_to_results = results_path,
    investorname = investor_name_placeholder,
    asset_type = "loans",
    level = calculation_level,
    file_type = "csv"
  )

  # Output loan book credit risk results
  loanbook_expected_loss <- loanbook_expected_loss %>%
    dplyr::select(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$id, .data$ald_sector,
      .data$equity_0_baseline, .data$equity_0_late_sudden, .data$debt,
      .data$volatility, .data$risk_free_rate, .data$term, .data$Survival_baseline,
      .data$Survival_late_sudden, .data$PD_baseline, .data$PD_late_sudden,
      .data$PD_change, .data$pd, .data$lgd, .data$percent_exposure, # TODO: keep all tehse PDs??
      .data$exposure_at_default, .data$expected_loss_baseline,
      .data$expected_loss_late_sudden
    ) %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$company_name, .data$ald_sector
    )

  loanbook_expected_loss %>%
    readr::write_csv(file.path(
      results_path,
      paste0("stress_test_results_lb_comp_el_", project_name, ".csv")
    ))

  # TODO: this is an unweighted average so far. keep in mind.
  loanbook_annual_pd_changes_sector <- loanbook_annual_pd_changes %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$year
    ) %>%
    dplyr::summarise(
      # ADO 2312 - weight the PD change by baseline equity because this represents the original exposure better
      PD_change = weighted.mean(x = .data$PD_change, w = .data$equity_t_baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$year
    )

  loanbook_annual_pd_changes_sector %>%
    readr::write_csv(file.path(
      results_path,
      paste0("stress_test_results_lb_sector_pd_changes_annual.csv")
    ))

  # TODO: this is an unweighted average so far. keep in mind.
  loanbook_overall_pd_changes_sector <- loanbook_expected_loss %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$term
    ) %>%
    dplyr::summarise(
      # ADO 2312 - weight the PD change by baseline equity because this represents the original exposure better
      PD_change = weighted.mean(x = .data$PD_change, w = .data$equity_0_baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      .data$scenario_geography, .data$scenario_name, .data$investor_name,
      .data$portfolio_name, .data$ald_sector, .data$term
    )

  loanbook_overall_pd_changes_sector %>%
    readr::write_csv(file.path(
      results_path,
      paste0("stress_test_results_lb_sector_pd_changes_overall.csv")
    ))
}
