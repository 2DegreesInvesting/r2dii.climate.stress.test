#' Run stress testing for bonds
#'
#' @return NULL
#' @export
run_stress_test_bonds <- function() {
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

  # Scenarios in the model_parameters.yml file must have the short names (SDS, NPS, etc)
  scenario_to_follow_baseline <- cfg_mod$scenarios$scenario_to_follow_baseline # sets which scenario trajectory the baseline scenario follows
  scenario_to_follow_ls <- cfg_mod$scenarios$scenario_to_follow_ls # sets which scenario trajectory LS scenario follows after shock period

  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls
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

  ###########################################################################
  # Load input datasets------------------------------------------------------
  ###########################################################################

  # Load company financial and production data-----------------------------------
  # ... get file paths for stresstest masterdata --------------------------------
  financial_data_bonds <- read_company_data(
    path = create_stressdata_masterdata_file_paths()$bonds,
    asset_type = "bonds"
  ) %>%
    wrangle_financial_data(start_year = start_year)

  # Load PACTA results / bonds portfolio------------------------
  bonds_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", paste0("Bonds_results_", calculation_level, ".rda"))

  pacta_bonds_results <- read_pacta_results(
    path = bonds_path,
    asset_type = "bonds",
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
    wrangle_and_check_sector_exposures_eq_cb(asset_type = "Bonds")

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

  # check scenario availability across data inputs for bonds
  check_scenario_availability(
    portfolio = pacta_bonds_results,
    scen_data = scenario_data,
    scenarios = scenarios_filter
  )

  # Prepare sector exposure data-------------------------------------------------
  # ...for bonds portfolio-------------------------------------------------------
  bonds_port_aum <- calculate_aum(sector_exposures)

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

  # Corporate bonds results -----------------------------------------------------

  bonds_results <- c()
  qa_annual_profits_cb <- c()
  bonds_expected_loss <- c()
  bonds_annual_pd_changes <- c()
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
      dplyr::mutate(Baseline = !!rlang::sym(scenario_to_follow_baseline)) %>%
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

    bonds_annual_profits <- pacta_bonds_results %>%
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
      bonds_annual_profits <- bonds_annual_profits %>%
        exclude_companies(
          exclusion = excluded_companies,
          scenario_baseline = scenario_to_follow_baseline,
          scenario_ls = scenario_to_follow_ls
        )
    }

    rows_bonds <- nrow(bonds_annual_profits)

    bonds_annual_profits <- bonds_annual_profits %>%
      dplyr::inner_join(financial_data_bonds,
        by = c("company_name", "id" = "corporate_bond_ticker", "ald_sector", "technology")
      )

    cat(
      "number of rows dropped by joining financial data on
      company_name, corporate_bond_ticker, ald_sector, and technology: ",
      rows_bonds - nrow(bonds_annual_profits), "\n"
    )
    # TODO: ADO 879 - note which companies are removed here, due to mismatch

    bonds_annual_profits <- bonds_annual_profits %>%
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

    bonds_annual_profits <- bonds_annual_profits %>%
      join_price_data(df_prices = df_prices) %>%
      calculate_net_profits() %>%
      dcf_model_techlevel(discount_rate = discount_rate)

    qa_annual_profits_cb <- qa_annual_profits_cb %>%
      dplyr::bind_rows(
        bonds_annual_profits %>%
          dplyr::mutate(year_of_shock = transition_scenario_i$year_of_shock)
      )

    plan_carsten_bonds <- pacta_bonds_results %>%
      dplyr::filter(
        .data$year == start_year,
        .data$technology %in% technologies_lookup,
        .data$scenario_geography == scenario_geography_filter,
        .data$scenario %in% .env$scenario_to_follow_ls
      )

    financial_data_bonds_pd <- financial_data_bonds %>%
      dplyr::select(company_name, corporate_bond_ticker, ald_sector, technology, pd)

    report_duplicates(
      data = financial_data_bonds_pd,
      cols = names(financial_data_bonds_pd)
    )

    rows_plan_carsten <- nrow(plan_carsten_bonds)

    plan_carsten_bonds <- plan_carsten_bonds %>%
      dplyr::inner_join(financial_data_bonds_pd, by = c("company_name", "id" = "corporate_bond_ticker", "ald_sector", "technology"))

    cat(
      "number of rows dropped from technology_exposure by joining financial data
      on company_name, corporate_bond_ticker, ald_sector and technology = ",
      rows_plan_carsten - nrow(plan_carsten_bonds), "\n"
    )
    # TODO: ADO 879 - note which companies are removed here, due to mismatch

    bonds_annual_profits <- bonds_annual_profits %>%
      dplyr::filter(!is.na(company_id))

    plan_carsten_bonds <- plan_carsten_bonds %>%
      dplyr::select(
        investor_name, portfolio_name, company_name, ald_sector, technology,
        scenario_geography, year, plan_carsten, plan_sec_carsten, term, pd
      )

    report_duplicates(
      data = plan_carsten_bonds,
      cols = names(plan_carsten_bonds)
    )

    bonds_results <- dplyr::bind_rows(
      bonds_results,
      company_asset_value_at_risk(
        data = bonds_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_bonds,
        port_aum = bonds_port_aum,
        flat_multiplier = 0.15,
        exclusion = excluded_companies
      )
    )

    bonds_overall_pd_changes <- bonds_annual_profits %>%
      calculate_pd_change_overall(
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)

    bonds_expected_loss <- dplyr::bind_rows(
      bonds_expected_loss,
      company_expected_loss(
        data = bonds_overall_pd_changes,
        loss_given_default = lgd_subordinated_claims,
        exposure_at_default = plan_carsten_bonds,
        port_aum = bonds_port_aum
      )
    )

    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient output from overall pd changes or related financial data inputs

    bonds_annual_pd_changes <- dplyr::bind_rows(
      bonds_annual_pd_changes,
      calculate_pd_change_annual(
        data = bonds_annual_profits,
        shock_year = transition_scenario_i$year_of_shock,
        end_of_analysis = end_year,
        risk_free_interest_rate = risk_free_rate
      )
    )
    # TODO: ADO 879 - note which companies produce missing results due to
    # insufficient input information (e.g. NAs for financials or 0 equity value)
  }

  write_stress_test_results(results = bonds_results,
                            expected_loss = bonds_expected_loss,
                            annual_pd_changes = bonds_annual_pd_changes,
                            asset_type = "bonds",
                            calculation_level = calculation_level)
}
