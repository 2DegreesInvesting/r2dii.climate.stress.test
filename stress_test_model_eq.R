###########################################################################
# Project Initialisation---------------------------------------------------
###########################################################################

library(tibble)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(forcats)
library(ggplot2)
library(highcharter)
library(stringr)
library(zoo)

source(file.path("R", "functions.R"))

function_paths <- c(
  "stress_test_model_functions.R",
  "0_global_functions_st.R",
  file.path(
    "R",
    c(
      "add_cols_result_df_pd_changes.R",
      "annual_pd_change_company_technology.R",
      "annual_pd_change_technology_shock_year.R",
      "apply_filters.R",
      "asset_value_at_risk.R",
      "calculate_annual_pd_changes.R",
      "calculate_aum.R",
      "calculate_overall_pd_changes.R",
      "create_empty_result_df_pd_changes.R",
      "company_asset_value_at_risk.R",
      "company_expected_loss.R",
      "convert_cap_to_generation.R",
      "exclude_companies.R",
      "extend_scenario_trajectory.R",
      "get_st_data_path.R",
      "interpolate_automotive_scenario.R",
      "lookup.R",
      "overall_pd_change_company_technology.R",
      "overall_pd_change_technology_shock_year.R",
      "qa_graphs_st.R",
      "read_capacity_factors.R",
      "read_company_data.R",
      "read_pacta_results.R",
      "read_price_data.R",
      "read_transition_scenarios.R",
      "set_paths.R",
      "set_tech_trajectories.R",
      "show_carbon_budget.R",
      "utils.R",
      "wrangle_and_check.R",
      "write_results.R"
    )
  )
)

source_all(function_paths)

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
set_project_paths(
  project_name = project_name,
  twodii_internal = twodii_internal,
  project_location_ext = project_location_ext
)

# Analysis Parameters----------------------------------------
# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

cfg <- config::get(file = file.path(project_location, "10_Parameter_File","AnalysisParameters.yml"))
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

###########################################################################
# Load input datasets------------------------------------------------------
###########################################################################

# Load company financial and production data-----------------------------------
# ... get file paths for stresstest masterdata --------------------------------
stresstest_masterdata_files <- create_stressdata_masterdata_file_paths(
  data_prep_timestamp = cfg$TimeStamps$DataPrep.Timestamp,
  twodii_internal = twodii_internal
)

# ... for equity---------------------------------------------------------------
financial_data_equity <- read_company_data(path = stresstest_masterdata_files$listed_equity,
                                           asset_type = "equity")

# Load PACTA results / equity portfolio------------------------
equity_path <- file.path(results_path, paste0("Equity_results_", calculation_level, ".rda"))

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
  )

# Load sector exposures of portfolio------------------------
sector_exposures <- readRDS(file.path(proc_input_path, "overview_portfolio.rda")) %>%
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
scen_data_file <- ifelse(twodii_internal == TRUE,
  path_dropbox_2dii("PortCheck", "00_Data", "01_ProcessedData", "03_ScenarioData", paste0("Scenarios_AnalysisInput_", start_year, ".csv")),
  file.path(data_location, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
)

# TODO: EITHER wrap check into more evocative function OR remove this when common format is agreed upon
if(twodii_internal == TRUE | start_year < 2020) {
  scenario_data <- readr::read_csv(scen_data_file, col_types = "ccccccccnnnncnnn") %>%
    filter(Indicator %in% c("Capacity", "Production", "Sales")) %>%
    filter(!(Technology == "RenewablesCap" & !is.na(Sub_Technology))) %>%
    select(-c(Sub_Technology, Indicator, AnnualvalIEAtech, refvalIEAtech, refvalIEAsec, mktFSRatio, techFSRatio)) %>%
    rename(
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
    mutate(scenario = str_replace(scenario, "NPSRTS", "NPS"))
} else {
  scenario_data <- readr::read_csv(scen_data_file, col_types = "ccccccncn") %>%
    rename(source = scenario_source)
}

scenario_data <- scenario_data %>%
  filter(source %in% c("ETP2017", "WEO2019")) %>% #TODO: this should be set elsewhere
  filter(!(source == "ETP2017" & ald_sector == "Power")) %>%
  mutate(scenario = ifelse(str_detect(scenario, "_"), str_extract(scenario, "[^_]*$"), scenario)) %>%
  check_scenario_timeframe(start_year = start_year, end_year = end_year)

# Correct for automotive scenario data error. CHECK IF ALREADY RESOLVED IN THE SCENARIO DATA, IF SO, DONT USE FUNCTION BELOW!
scenario_data <- scenario_data %>%
  correct_automotive_scendata(interpolation_years = c(2031:2034, 2036:2039)) %>%
  filter(
    ald_sector %in% sectors_lookup &
      technology %in% technologies_lookup &
      scenario_geography == scenario_geography_filter)

# Load price data----------------------------------------
df_price <- read_price_data(
    path = file.path(data_location, paste0("prices_data_", price_data_version, ".csv")),
    version = "old",
    expected_technologies = technologies_lookup
  ) %>%
  filter(year >= start_year) %>%
  check_price_consistency()

# Load excluded companies-------------------------------
if (company_exclusion) {
  excluded_companies <- readr::read_csv(
    file.path(data_location, "exclude-companies.csv"),
    col_types = "cc"
  )
}

###########################################################################
# Data wrangling / preparation---------------------------------------------
###########################################################################

# Prepare net profit margins equity----------------------

financial_data_equity <- financial_data_equity %>%
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
  # ADO 879 - remove year and production/EFs to simplify joins that do not need yearly variation yet
  dplyr::filter(.data$year == .env$start_year) %>%
  dplyr::select(
    -c(
      .data$year, .data$ald_production_unit, .data$ald_production,
      .data$ald_emissions_factor_unit, .data$ald_emissions_factor
    )
  )
#TODO: any logic/bounds needed for debt/equity ratio and volatility?

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

for (i in seq(1, nrow(transition_scenarios))) {
  transition_scenario_i <- transition_scenarios[i, ]
  year_of_shock <- transition_scenario_i$year_of_shock
  duration_of_shock <- transition_scenario_i$duration_of_shock
  overshoot_method <- transition_scenario_i$overshoot_method
  use_prod_forecasts_baseline <- transition_scenario_i$use_prod_forecasts_baseline
  use_prod_forecasts_ls <- transition_scenario_i$use_prod_forecasts_ls

  # Create shock scenario dataframe for scenario i
  # For now we use the old shock scenario dataframe format. Should change this over time as its far from optimal
  shock_scenario <- create_shock_scenario(transition_scenario = transition_scenario_i)

  # Calculate late and sudden prices for scenario i
  df_prices <- df_price %>%
    mutate(Baseline = NPS) %>% # FIXME this should be parameterized!!
    rename(
      year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
      SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
    ) %>%
    group_by(ald_sector, technology) %>%
    #### OPEN: Potentially a problem with the LS price calculation. Concerning warning
    mutate(
      late_sudden_price = late_sudden_prices(SDS_price = SDS_price, Baseline_price = Baseline_price, overshoot_method = overshoot_method)
    ) %>%
    ungroup()

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
      scenario_to_follow_ls_aligned = scenario_to_follow_ls_aligned,
      start_year = start_year,
      end_year = end_year,
      analysis_time_frame = time_horizon
    )

  if (exists("excluded_companies")) {
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

  cat("number of rows dropped by joining financial data on
      company_name, ald_sector and technology = ",
      rows_equity - nrow(equity_annual_profits), "\n")
  # TODO: ADO 879 - note which companies are removed here, due to mismatch

  equity_annual_profits <- equity_annual_profits %>%
    arrange(
      scenario_name, investor_name, portfolio_name, scenario_geography, id,
      company_name, ald_sector, technology, year
    ) %>%
    group_by(
      scenario_name, investor_name, portfolio_name, scenario_geography, id,
      company_name, ald_sector, technology
    ) %>%
    # NOTE: this assumes emissions factors stay constant after forecast and prod not continued
    tidyr::fill(
      company_id, pd, net_profit_margin, debt_equity_ratio, volatility,
      ald_emissions_factor, ald_emissions_factor_unit, ald_production_unit,
      .direction = "down"
    ) %>%
    ungroup()

  equity_annual_profits <- equity_annual_profits %>%
    join_price_data(df_prices = df_prices) %>%
    # join_net_profit_margins(net_profit_margins = net_profit_margins) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate)

  qa_annual_profits_eq <- qa_annual_profits_eq %>%
    bind_rows(
      equity_annual_profits %>%
        mutate(year_of_shock = transition_scenario_i$year_of_shock)
    )

  plan_carsten_equity <- pacta_equity_results %>%
    filter(
      year == start_year,
      technology %in% technologies_lookup,
      scenario_geography == scenario_geography_filter
    )

  plan_carsten_equity <- plan_carsten_equity %>%
    select(
      investor_name, portfolio_name, company_name, ald_sector, technology,
      scenario_geography, year, plan_carsten, plan_sec_carsten
    ) %>%
    distinct(across(everything()))

  if (!exists("excluded_companies")) {
    equity_results <- bind_rows(
      equity_results,
      company_asset_value_at_risk(
        data = equity_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_equity,
        port_aum = equity_port_aum,
        flat_multiplier = 1,
        exclusion = NULL
      )
    )
  } else {
    equity_results <- bind_rows(
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
  }
}

# Output equity results
equity_results %>% write_results(
  path_to_results = results_path,
  investorname = investor_name_placeholder,
  asset_type = "equity",
  level = calculation_level,
  file_type = "csv"
)
