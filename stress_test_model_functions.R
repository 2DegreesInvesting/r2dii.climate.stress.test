#######################
# Create shock scenario
#######################
create_shock_scenario <- function(transition_scenario) {
  if (transition_scenario$overshoot_method) {
    # print('Integral method selected for calculation of late&sudden production scenarios.
    #       Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory')
    tibble(
      "scenario_name" = transition_scenario$scenario_name,
      "year_of_shock" = transition_scenario$year_of_shock,
      "duration_of_shock" = 2040 - transition_scenario$year_of_shock + 1,
      "Coal" = NA,
      "Oil" = NA,
      "Gas" = NA,
      "GasCap" = NA,
      "RenewablesCap" = NA,
      "NuclearCap" = NA,
      "CoalCap" = NA,
      "HydroCap" = NA,
      "OilCap" = NA,
      "ICE" = NA,
      "Electric" = NA,
      "Hybrid" = NA
    )
  } else {
    print("technology production shocks set by user (overshoot_method==FALSE)")

    tibble(
      "scenario_name" = transition_scenario$scenario_name,
      "year_of_shock" = transition_scenario$year_of_shock,
      "duration_of_shock" = transition_scenario$duration_of_shock,
      "Coal" = transition_scenario$Coal,
      "Oil" = transition_scenario$Oil,
      "Gas" = transition_scenario$Gas,
      "GasCap" = transition_scenario$GasCap,
      "RenewablesCap" = transition_scenario$RenewablesCap,
      "NuclearCap" = transition_scenario$NuclearCap,
      "CoalCap" = transition_scenario$CoalCap,
      "HydroCap" = transition_scenario$HydroCap,
      "OilCap" = transition_scenario$OilCap,
      "ICE" = transition_scenario$ICE,
      "Electric" = transition_scenario$Electric,
      "Hybrid" = transition_scenario$Hybrid
    )
  }
}

#################################
# INTERPOLATION MISSING scenario DATAPOINTS
#################################

interpolate_scenario_prod <- function(df) {
  # ------ SPLINE INTERPOLATION OF scenario PRODUCTION VALUES -------------------
  # scenario values are only given in 5 year intervals
  # Following code changes the format of the portcheck company result file, adds missing years (NA values) and interpolates the missing values with spline interpolation
  # IMPORTANT NOTE:
  # Currently only works when the start year AND the end year have scenario values, otherwise the interpolation throws an error!
  if (max(df$year) != 2040) {
    print("You need to run function extend_scen_traj() first, scenario values up until 2040 are missing")
    stop()
  }

  if (!"id" %in% names(df)) {
    df$id <- "PortfolioLevel"
  }

  if (!"company_name" %in% names(df)) {
    df$company_name <- "PortfolioLevel"
  }
  df %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c(
        "investor_name", "portfolio_name", "id", "company_name",
        "year", "scenario_geography", "ald_sector", "technology",
        "plan_tech_prod"
      ),
      names_from = scenario, values_from = scen_tech_prod
    ) %>%
    complete(
      investor_name = unique(df$investor_name),
      portfolio_name = unique(df$portfolio_name),
      id = unique(df$id),
      company_name = unique(df$company_name),
      ald_sector = sectors,
      scenario_geography = scenario_geography,
      technology = technologies,
      year = start_year:end_year,
      fill = list(Value = NA)
    ) %>%
    arrange(id, company_name, scenario_geography, ald_sector, technology, year) %>%
    group_by(id, company_name, ald_sector, technology, scenario_geography) %>%
    mutate(drop = sum(plan_tech_prod, na.rm = TRUE)) %>%
    filter(!is.na(drop) & drop > 0) %>%
    select(-drop) %>%
    mutate(
      SDS = na.approx(SDS),
      NPSRTS = na.approx(NPSRTS),
      CPS = ifelse(ald_sector == "Automotive", NA, na.approx(CPS)) # na.approx needs values at beginning (start_year) and end (end_year),
      # otherwise it throws an error. cps doesnt exist for automotive,
      # hence this line to prevent an error
      # B2DS = na.approx(B2DS)
    )
}



# STILL TO FIX: five year moving average

f <- function(shock_strength_calc) {
  sum(scen_to_follow[1:(position_shock_year + duration_of_shock - 1)]) -
    sum(late_and_sudden[1:(position_shock_year - 1)]) -
    late_and_sudden[position_shock_year - 1] * sum(seq(1, duration_of_shock)) * sum(seq(1, duration_of_shock)) * (100 - shock_strength_calc / 100) -
    (length(late_and_sudden) - (position_shock_year + duration_of_shock) + 1) *
      (scen_to_follow[duration_of_shock + position_shock_year - 1] -
        late_and_sudden[position_shock_year - 1] * (100 + duration_of_shock * shock_strength_calc) / 100)
}



# LATE AND SUDDEN PRICES ----------------------------------------

late_sudden_prices <- function(SDS_price, Baseline_price, overshoot_method) {
  # input:
  # vector with SDS and Baseline (NPS) prices
  # Calculates late and sudden prices based on these two vectors
  # At the moment LS prices follow SDS prices until shricesock, then LS prices linearly reach NPS prices during the shock period. After shock period, LS prices follow SDS prices
  position_shock_year <- year_of_shock - start_year + 1
  ls_price <- Baseline_price

  baseline_price_at_shock <- Baseline_price[0 + position_shock_year]
  SDS_price_end_shockperiod <- SDS_price[duration_of_shock + position_shock_year - 1]

  ls_price[position_shock_year:(position_shock_year + duration_of_shock - 1)] <- na.approx(c(baseline_price_at_shock, rep(NA, duration_of_shock - 2), SDS_price_end_shockperiod))
  if (!overshoot_method) {
    ls_price[(position_shock_year + duration_of_shock):length(ls_price)] <- SDS_price[(position_shock_year + duration_of_shock - 1):length(SDS_price)]
  }

  return(ls_price)
}


loadConfig <- function() {
  # Load configuration file
  config::get(file = file.path(location_credit_methodology, "Scripts","workflow-generic.yml"))
}


create_stresstest_folders <- function(
                                      date) {
  folder_loc <- "./StressTesting/"

  if (!dir.exists(folder_loc)) {
    dir.create(folder_loc, recursive = TRUE)
  }

  if (!dir.exists(paste0(folder_loc, date))) {
    dir.create(paste0(folder_loc, date), recursive = TRUE)

    for (subfolder in c("Data", "Scripts", "Results")) {
      dir.create(paste0(folder_loc, date, "/", subfolder), recursive = TRUE)
    }
  }
}

# COMPANY ANNUAL PROFITS BY TECH --------------------------------------------------

net_profit_margin_setup <- function(net_profit_margin_coal,
                                    net_profit_margin_coalcap,
                                    net_profit_margin_electric,
                                    net_profit_margin_gas,
                                    net_profit_margin_gascap,
                                    net_profit_margin_hybrid,
                                    net_profit_margin_ice,
                                    net_profit_margin_nuclearcap,
                                    net_profit_margin_oil,
                                    net_profit_margin_renewablescap,
                                    net_profit_margin_hydrocap,
                                    net_profit_margin_oilcap) {
  tibble(
    "technology" = c("Coal", "CoalCap", "Electric", "Gas", "GasCap", "Hybrid", "ICE", "NuclearCap", "Oil", "RenewablesCap", "HydroCap", "OilCap"),
    "net_profit_margin" = c(
      net_profit_margin_coal,
      net_profit_margin_coalcap,
      net_profit_margin_electric,
      net_profit_margin_gas,
      net_profit_margin_gascap,
      net_profit_margin_hybrid,
      net_profit_margin_ice,
      net_profit_margin_nuclearcap,
      net_profit_margin_oil,
      net_profit_margin_renewablescap,
      net_profit_margin_hydrocap,
      net_profit_margin_oilcap
    )
  )
}

join_price_data <- function(df, df_prices) {
  # Joins price data by sector, technology, year
  # scenario_geography NOT YET INCLUDED!
  df %>%
    left_join(df_prices, by = c("technology", "ald_sector", "year"))
}

join_net_profit_margins <- function(df, net_profit_margins) {
  # Joins net profit margins by technology
  df %>%
    left_join(net_profit_margins, by = "technology")
}

calculate_net_profits <- function(df) {
  # Calculates annual net profits
  # Input: dataframe that has the baseline & LS production, prices and technoogy net profit margins
  df %>%
    mutate(
      net_profits_ls = late_sudden * late_sudden_price * net_profit_margin,
      net_profits_baseline = baseline * Baseline_price * net_profit_margin
    )
}


Merton_solve <- function(parm) {
  # function that we will minimize in order to obtain
  # V0, the value of company’s assets today, and sigmaV, assets’ volatility

  V0 <- parm[1] # initial value for V0
  sigmaV <- parm[2] # initial value for sigmaV


  d1 <- (log(V0 / B) + (r + sigmaV^2 / 2) * maturity_T) / (sigmaV * sqrt(maturity_T))
  d2 <- d1 - sigmaV * sqrt(maturity_T)
  G <- V0 * pnorm(d1) - B * exp(-r * maturity_T) * pnorm(d2) - S0
  H <- pnorm(d1) * sigmaV * V0 - sigmaS * S0

  return(G^2 + H^2)
}

dcf_model_techlevel <- function(data, discount_rate) {
  # Calculates the annual discounted net profits on technology level
  data %>%
    group_by(investor_name, portfolio_name, id, company_name, ald_sector, technology, scenario_geography) %>%
    mutate(
      t_calc = seq(0, (n() - 1)),
      discounted_net_profit_baseline = net_profits_baseline / (1 + discount_rate)^t_calc,
      discounted_net_profit_ls = net_profits_ls / (1 + discount_rate)^t_calc
    ) %>%
    select(-t_calc)
}

dcf_model_sectorlevel <- function(data, discount_rate) {
  # Calculates the annual discounted net profits on technology level
  data %>%
    group_by(id, ald_sector, scenario_geography) %>%
    mutate(
      t_calc = seq(0, (n() - 1)),
      discounted_net_profit_baseline = net_profits_baseline / (1 + discount_rate)^t_calc,
      discounted_net_profit_ls = net_profits_ls / (1 + discount_rate)^t_calc
    ) %>%
    select(-t_calc)
}

corp_lbk_value_at_risk <- function(df, shock_scenario, equity_loan_factor, plan_carsten, lbk_exposures) {
  # Temporary value at risk calculation for corporate loanbook, both on sector and tech level
  # Calculates sector total discounted net present value under a late & sudden and baseline scenario
  # Calculates the value at risk (difference in disc NPV late and sudden and baseline)
  df %>%
    ungroup() %>%
    mutate(ald_sector = ifelse(technology %in% c("Oil", "Gas", "Coal"), "Fossil Fuels", ald_sector)) %>%
    filter(
      year >= shock_scenario$year_of_shock,
      !is.na(discounted_net_profit_ls),
      !is.na(discounted_net_profit_baseline)
    ) %>%
    group_by(ald_sector, technology, scenario_geography) %>%
    summarise(
      total_disc_npv_ls = sum(discounted_net_profit_ls) * (1 + terminal_value),
      total_disc_npv_baseline = sum(discounted_net_profit_baseline) * (1 + terminal_value),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    mutate(VaR_technology = 100 * equity_loan_factor * (total_disc_npv_ls - total_disc_npv_baseline) / total_disc_npv_baseline) %>%
    select(-c(total_disc_npv_ls, total_disc_npv_baseline)) %>%
    left_join(plan_carsten, by = "technology") %>%
    group_by(ald_sector, scenario_geography) %>%
    mutate(
      VaR_sector = sum(VaR_technology * plan_carsten) / sum(plan_carsten),
      scenario_name = shock_scenario$scenario_name
    ) %>%
    select(-plan_carsten) %>%
    left_join(lbk_exposures, by = "ald_sector") %>%
    mutate(sector_loss = exposure * VaR_sector / 100) %>%
    rename(sector_exposure = exposure) %>%
    left_join(shock_scenario %>%
      pivot_longer(
        cols = -c("scenario_name", "year_of_shock", " duration_of_shock"),
        names_to = "technology", values_to = "production_shock_perc"
      ),
    by = c("scenario_name", "technology")
    )
}





stress_test_model_wrapper <- function(
                                      cb_portfolioinput_full,
                                      portcheck_compresults_bonds_full,
                                      equity_portfolioinput_full,
                                      investorname_equity,
                                      portcheck_portresults_equity_full,
                                      investorname_bonds,
                                      portfolioname_equity,
                                      portfolioname_bonds,
                                      capacity_factors_power,
                                      net_profit_margins,
                                      shock_scenario,
                                      discount_rate,
                                      div_netprofit_prop_coef,
                                      recovery_rate) {
  cb_portfolioinput <- cb_portfolioinput_full %>%
    filter(
      investor_name == investorname_bonds,
      portfolio_name == portfolioname_bonds,
      mapped_sector != "Other"
    )

  portcheck_compresults_bonds <- apply_filters(portcheck_compresults_bonds_full, investorname_bonds, portfolioname_bonds)

  equity_portfolioinput <- equity_portfolioinput_full %>%
    filter(
      investor_name == investorname_equity,
      portfolio_name == portfolioname_equity
    )

  portcheck_portresults_equity <- apply_filters(portcheck_portresults_equity_full, investorname_equity, portfolioname_equity) %>%
    filter(
      allocation == allocation_method_equity,
      equity_market == equity_market
    )

  equity_port_aum <- unique(portcheck_portresults_equity$Port.AUM)
  bonds_port_aum <- unique(portcheck_compresults_bonds$Port.AUM)

  start_year <- min(portcheck_compresults_bonds$year)

  portcheck_portresults_equity <- portcheck_portresults_equity %>%
    ungroup() %>%
    left_join(capacity_factors_power, by = "technology") %>%
    mutate(
      plan_tech_prod = ifelse(ald_sector == "Power", plan_tech_prod * capacity_factor * 24 * 365, plan_tech_prod),
      scen_tech_prod = ifelse(ald_sector == "Power", scen_tech_prod * capacity_factor * 24 * 365, scen_tech_prod),
      scenario_geography = scenario_geography
    ) %>%
    select(-capacity_factor)

  portcheck_compresults_bonds <- portcheck_compresults_bonds %>%
    ungroup() %>%
    left_join(capacity_factors_power, by = "technology") %>%
    mutate(
      plan_tech_prod = ifelse(ald_sector == "Power", plan_tech_prod * capacity_factor * 24 * 365, plan_tech_prod),
      scen_tech_prod = ifelse(ald_sector == "Power", scen_tech_prod * capacity_factor * 24 * 365, scen_tech_prod),
      scenario_geography = scenario_geography
    ) %>% # rename globalaggregate of power to global, even though the scenario values represent global aggregate, so that the functions work
    select(-capacity_factor)

  portcheck_compresults_bonds_stresstest <- interpolate_scenario_prod(portcheck_compresults_bonds)
  portcheck_portresults_equity_stresstest <- interpolate_scenario_prod(portcheck_portresults_equity)

  # Set baseline trajectory
  portcheck_compresults_bonds_stresstest <- set_baseline_trajectory(portcheck_compresults_bonds_stresstest, scenario_to_follow_baseline, use_prod_forecasts_baseline)
  portcheck_portresults_equity_stresstest <- set_baseline_trajectory(portcheck_portresults_equity_stresstest, scenario_to_follow_baseline, use_prod_forecasts_baseline)

  # Set Late & Sudden trajectory

  portcheck_compresults_bonds_stresstest <- set_ls_trajectory(portcheck_compresults_bonds_stresstest, scenario_to_follow_ls, shock_scenario, use_prod_forecasts_ls, overshoot_method)
  portcheck_portresults_equity_stresstest <- set_ls_trajectory(portcheck_portresults_equity_stresstest, scenario_to_follow_ls, shock_scenario, use_prod_forecasts_ls, overshoot_method)

  # Join in price data, calculate revenue, calculate net profits
  portcheck_compresults_bonds_stresstest <- join_price_data(portcheck_compresults_bonds_stresstest, df_prices)
  portcheck_compresults_bonds_stresstest <- join_net_profit_margins(portcheck_compresults_bonds_stresstest, net_profit_margins)
  portcheck_compresults_bonds_stresstest <- calculate_net_profits(portcheck_compresults_bonds_stresstest)

  portcheck_portresults_equity_stresstest <- join_price_data(portcheck_portresults_equity_stresstest, df_prices)
  portcheck_portresults_equity_stresstest <- join_net_profit_margins(portcheck_portresults_equity_stresstest, net_profit_margins)
  portcheck_portresults_equity_stresstest <- calculate_net_profits(portcheck_portresults_equity_stresstest)

  # We aggregate the company profits by scenario_geography and year (so we aggregate company the profits over all technologies a company produces in)

  portcheck_portresults_equity_stresstest <- dcf_model_techlevel(portcheck_portresults_equity_stresstest, discount_rate)

  # Run discounted cash flow model equity
  equity_value_at_risk_tech <- asset_value_at_risk(portcheck_portresults_equity_stresstest, terminal_value, shock_scenario$year_of_shock, div_netprofit_prop_coef)

  # Calculate technology value at risk
  equity_tech_exposures <- portcheck_portresults_equity %>%
    filter(year == 2019, scenario_geography == scenario_geography) %>%
    distinct(technology, plan_carsten) %>%
    mutate(plan_carsten_norm = plan_carsten / sum(plan_carsten))

  # Equity portfolio losses chart calculations
  equity_portfolio_var <- equity_value_at_risk_tech %>%
    left_join(equity_tech_exposures, by = "technology") %>%
    mutate(
      climate_rel_calc = VaR_technology * plan_carsten_norm,
      port_calc = VaR_technology * plan_carsten
    ) %>%
    filter(!is.na(VaR_technology)) %>%
    ungroup() %>%
    summarise(
      climate_relevant_var = sum(climate_rel_calc),
      portfolio_var = sum(port_calc),
      climate_relevant_exposure = sum(plan_carsten),
      .groups = "drop_last"
    ) %>%
    mutate(
      current_portfolio_aum = equity_port_aum,
      future_portfolio_aum = current_portfolio_aum * (1 + (portfolio_var / 100)),
      current_climate_relevant_aum = climate_relevant_exposure * current_portfolio_aum,
      current_other_sectors_aum = (1 - climate_relevant_exposure) * current_portfolio_aum,
      future_climate_relevant_aum = climate_relevant_exposure * future_portfolio_aum,
      future_other_sectors_aum = (1 - climate_relevant_exposure) * future_portfolio_aum
    )

  # # Corporate bonds stress test results -----------------------------------

  cb_portfolioinput_sel <- cb_portfolioinput %>%
    filter(id %in% unique(portcheck_compresults_bonds_stresstest$id)) %>%
    distinct(id, mapped_sector, ISIN, MarketValue, Currency, coupon_value, years_to_maturity, amount_issued) %>%
    mutate(
      years_to_maturity = ifelse(is.na(years_to_maturity), ceiling(mean(years_to_maturity, na.rm = T)), ceiling(years_to_maturity)),
      coupon_value = ifelse(is.na(coupon_value), 5, coupon_value)
    )

  # calculate bond % marketvalue loss using the simplistic model
  bond_losses <- cb_portfolioinput_sel %>%
    filter(id %in% unique(portcheck_compresults_bonds_stresstest$id)) %>%
    mutate(years_to_maturity = ifelse(years_to_maturity > (end_year - start_year + 1), end_year - start_year + 1, years_to_maturity)) %>%
    group_by(id, ISIN, years_to_maturity) %>%
    mutate(
      sum_np_ls = sum(portcheck_compresults_bonds_stresstest[portcheck_compresults_bonds_stresstest$id == unique(id) & portcheck_compresults_bonds_stresstest$ald_sector == unique(mapped_sector), ]$net_profits_ls[seq(1, years_to_maturity)]),
      sum_np_baseline = sum(portcheck_compresults_bonds_stresstest[portcheck_compresults_bonds_stresstest$id == unique(id) & portcheck_compresults_bonds_stresstest$ald_sector == unique(mapped_sector), ]$net_profits_baseline[seq(1, years_to_maturity)])
    ) %>%
    arrange(id) %>%
    ungroup() %>%
    mutate(percentage_loss = (1 - coupon_value / 100) * (recovery_rate^0.1) * (100 * (sum_np_ls - sum_np_baseline) / sum_np_baseline)) # apply model

  # Calculate sector value at risk
  bonds_value_at_risk_sector <- bond_losses %>%
    filter(!is.na(percentage_loss)) %>%
    group_by(mapped_sector) %>%
    summarise(
      VaR_sector = sum(percentage_loss * MarketValue) / sum(MarketValue),
      .groups = "drop_last"
    ) %>%
    rename(ald_sector = mapped_sector)

  # Get sector exposures from results file (since we only loaded the company results file we aggregate plan_sec_carsten after applying several filter to prevent double counting)
  bonds_sector_exposures <- portcheck_compresults_bonds %>%
    filter(year == 2019, scenario_geography == scenario_geography) %>%
    distinct(id, ald_sector, .keep_all = T) %>%
    group_by(ald_sector) %>%
    summarise(
      plan_sec_carsten = sum(plan_sec_carsten),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    mutate(plan_sec_carsten_norm = plan_sec_carsten / sum(plan_sec_carsten))

  bonds_portfolio_var <- bonds_value_at_risk_sector %>%
    left_join(bonds_sector_exposures, by = "ald_sector") %>%
    mutate(
      climate_rel_calc = VaR_sector * plan_sec_carsten_norm,
      port_calc = VaR_sector * plan_sec_carsten
    ) %>%
    ungroup() %>%
    summarise(
      climate_relevant_var = sum(climate_rel_calc),
      portfolio_var = sum(port_calc),
      climate_relevant_exposure = sum(plan_sec_carsten),
      .groups = "drop_last"
    ) %>%
    mutate(
      current_portfolio_aum = bonds_port_aum,
      future_portfolio_aum = current_portfolio_aum * (1 + (portfolio_var / 100)),
      current_climate_relevant_aum = climate_relevant_exposure * current_portfolio_aum,
      current_other_sectors_aum = (1 - climate_relevant_exposure) * current_portfolio_aum,
      future_climate_relevant_aum = climate_relevant_exposure * future_portfolio_aum,
      future_other_sectors_aum = (1 - climate_relevant_exposure) * future_portfolio_aum
    )

  # Portfolio losses chart results:
  portfolio_losses <- bind_rows(equity_portfolio_var, bonds_portfolio_var)

  results <- bind_rows(shock_scenario, shock_scenario) %>%
    mutate(asset_type = c("Equity", "Bonds")) %>%
    cbind(portfolio_losses)


  return(results)
}

# run basic portfolio data consistency checks that are required for further data processing
check_portfolio_consistency <- function(df) {
  # first year in data set must be the same as start year defined in parameters
  if (df %>% pull(year) %>% min(na.rm = TRUE) != start_year) {
    write_log("Start year of the analysis and first year in the input portfolio do not match.
              This will lead to errors in the stress test calculations.")
    stop("Start year of the analysis and first year in the input portfolio do not match.")
  }
  return(df)
}

check_scenario_consistency <- function(df) {
  # the year of shock must be greater or equal to the start year of the analysis
  if (!all(df %>% pull(year_of_shock) >= start_year)) {
    write_log("Year of shock out of bounds. Shock cannot happen before the start year of the anaylsis.")
    stop("Year of shock out of bounds. Shock cannot happen before the start year of the anaylsis.")
  }
  return(df)
}

check_price_consistency <- function(df) {
  # the year of shock must be greater or equal to the start year of the analysis
  if (!all(df %>% pull(year) >= start_year)) {
    write_log("Timerange for price data out of bounds. Past prices cannot be included in the further analysis.")
    stop("Timerange for price data out of bounds. Past prices cannot be included in the further analysis.")
  }
  return(df)
}

check_scenario_availability <- function(portfolio, scen_data, scenarios = scenarios) {
  # check that scenarios in portfolio are allowed
  if (!all(portfolio %>% pull(scenario) %>% unique() %in% scenarios)) {
    write_log("Some scenarios in this data frame are not in the list of allowed scenarios. Please check!")
    stop("Some scenarios in this data frame are not in the list allowed of scenarios. Please check!")
  }
  # check that at least two allowed scenarios remain in portfolio
  if (length(portfolio %>% pull(scenario) %>% unique()) < 2) {
    write_log("There are less than two allowed scenarios in the portfolio. Stress test requires at least two!")
    stop("There are less than two allowed scenarios in the portfolio. Stress test requires at least two!")
  }
  # check scenarios in portfolio correspond to scenarios in scen data
  if (!all(portfolio %>% pull(scenario) %>% unique() %in% (scen_data %>% pull(scenario) %>% unique()))) {
    write_log("Scenarios differ between portfolio and scenario trajectory data. Check if correct inputs were used.")
    stop("Scenarios differ between portfolio and scenario trajectory data. Check if correct inputs were used.")
  }
}

# check if the imported scenario data covers every year within the timeframe of analysis
check_scenario_timeframe <- function(scenario_data, start_year = start_year, end_year = end_year) {
  if (!all(seq(start_year, end_year) %in% (scenario_data %>% pull(year) %>% unique()))) {
    write_log(paste0("Imported scenario data does not cover the full time frame of the analysis. Scenario data must cover at least the period from ", start_year, " to ", end_year))
    stop(paste0("Imported scenario data does not cover the full time frame of the analysis. Scenario data must cover at least the period from ", start_year, " to ", end_year))
  } else {
    return(scenario_data)
  }
}

# check if the scenarios selected in the stress test project at hand
# are compatible with the scenarios passed from the PACTA results input
check_scenario_settings <- function(portfolio, scenario_selections = scenarios) {
  if (!any(scenarios %in% (portfolio %>% pull(.data$scenario) %>% unique()))) {
    stop(
      paste0(
        "Error: scenarios selected for anaylsis (",
        paste0(scenario_selections, collapse = ", "),
        ") and scenarios provided in the analysed portfolio (",
        paste0(portfolio %>% pull(.data$scenario) %>% unique(), collapse = ", "),
        ") differ."
      ), call. = FALSE
    )
  } else {
    portfolio
  }
}
