## Project Initialisation

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
      "apply_filters.R",
      "asset_value_at_risk.R",
      "company_asset_value_at_risk.R",
      "convert_cap_to_generation.R",
      "exclude_companies.R",
      "extend_scenario_trajectory.R",
      "get_st_data_path.R",
      "interpolate_automotive_scenario.R",
      "qa_graphs_st.R",
      "read_capacity_factors.R",
      "set_paths.R",
      "set_tech_trajectories.R",
      "show_carbon_budget.R",
      "utils.R",
      "write_results.R"
    )
  )
)

source_all(function_paths)

################
# INPUT VARIABLES
################

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
calculation_level <- cfg_st$calculation_level
company_exclusion <- cfg_st$company_exclusion

data_location <- file.path(get_st_data_path(), data_path())

# set input path
set_project_paths(
  project_name = project_name,
  twodii_internal = twodii_internal,
  project_location_ext = project_location_ext
)


# THIS NEEDS TO BE INVESTIGATED! PROBABLY LOOP OVER INV OR ALLOW SPECIFICATION IN CONFIG
investorname_bonds <- "Meta Investor" #' Fixed Income Index'  #'Meta Portfolio'
investorname_equity <- "Meta Investor" #' Equity Index'


#### Analysis Parameters----------------------------------------
# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

cfg <- config::get(file = file.path(project_location, "10_Parameter_File","AnalysisParameters.yml"))
# OPEN: check_valid_cfg() not applicable here
start_year <- cfg$AnalysisPeriod$Years.Startyear
dataprep_timestamp <- cfg$TimeStamps$DataPrep.Timestamp # is this being used for anything???


##### Filters----------------------------------------
# The filter settings should comply with the filters from the parent PACTA project as per default
# There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
# move to config once mechanism to include/exclude filters from original pacta project exists

#### OPEN: This could largely be taken from cfg file. No apparent reason why not.
scenario_geography_filter <- "Global"
# scenario_geography_filter <- cfg$Lists$Scenario.Geography.List

# ALLOW ONLY precisely the scenarios that are supposed to be kept from the portfolio and scen_data
# NOTE scenarios from the same source, same secenario name and diff years will likely fail
# E.g. WEO2019_SDS AND WEO2020_SDS will produce near-duplicates that break the analysis
scenarios <- c(
  "B2DS",
  "CPS",
  "NPS",
  "NPSRTS",
  "SDS"#,
  # "ETP2017_B2DS",
  # "ETP2017_NPS",
  # "ETP2017_SDS",
  # "GECO2019_1.5c",
  # "GECO2019_2c_m",
  # "GECO2019_ref",
  # "WEO2019_CPS",
  # "WEO2019_NPS",
  # "WEO2019_SDS" # ,
  # "WEO2020_NPS",
  # "WEO2020_SDS"
)
# scenarios <- cfg$Large.Universe.Filter$SCENARIO.FILTER

allocation_method_equity <- "portfolio_weight"
equity_market_filter <- cfg$Lists$Equity.Market.List

sectors <- c("Power", "Oil&Gas", "Coal", "Automotive")
# setors <- cfg$Large.Universe.Filter$SECTOR.FILTER
technologies <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)
# technologies <- cfg$Lists$Technology.List

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

recovery_rate <- cfg_mod$financials$recovery_rate # Bonds recovery rate, set to 38%, historical recovery rate of senior bonds (Moody's 2017). See Storm ahead paper page 39
discount_rate <- cfg_mod$financials$discount_rate # Discount rate
##### OPEN: this needs to be estimated based on data
terminal_value <- cfg_mod$financials$terminal_value
div_netprofit_prop_coef <- cfg_mod$financials$div_netprofit_prop_coef # determine this value using bloomberg data

# technology net profit margins
## the parameters should be outsorced into a config file at some point
## they should also be looped over, if multiple scenarios are to be analysed
net_profit_margin_coal <- cfg_mod$net_profit_margin$coal
net_profit_margin_coalcap <- cfg_mod$net_profit_margin$coalcap
net_profit_margin_electric <- cfg_mod$net_profit_margin$electric
net_profit_margin_gas <- cfg_mod$net_profit_margin$gas
net_profit_margin_gascap <- cfg_mod$net_profit_margin$gascap
net_profit_margin_hybrid <- cfg_mod$net_profit_margin$hybrid
net_profit_margin_ice <- cfg_mod$net_profit_margin$ice
net_profit_margin_nuclearcap <- cfg_mod$net_profit_margin$nuclearcap
net_profit_margin_oil <- cfg_mod$net_profit_margin$oil
net_profit_margin_oilcap <- cfg_mod$net_profit_margin$oilcap
net_profit_margin_renewablescap <- cfg_mod$net_profit_margin$renewablescap
net_profit_margin_hydrocap <- cfg_mod$net_profit_margin$hydrocap



###############
# Load input datasets----------------------------------------

# OPEN:: IMPORTANT: ADD input data consistency checks. E.G. the start year of the iput data must be the same as the start year set in the project parameters

# portcheck_portresults_bonds_full <- readRDS(file.path(results_path, investorname_bonds, "Bonds_results_portfolio.rda")) %>%
portcheck_portresults_bonds_full <- readRDS(file.path(results_path, investorname_bonds, paste0("Bonds_results_", calculation_level, ".rda"))) %>%
  select(-c(trajectory_deviation, trajectory_alignment, scen_tech_share, plan_tech_share, scen_sec_emissions_factor, scen_sec_carsten, scen_alloc_wt_sec_prod, scen_sec_prod, plan_sec_emissions_factor, scen_emission_factor, scen_carsten, plan_emission_factor)) %>%
  filter(!is.na(scenario)) %>%
  check_scenario_settings(scenario_selections = scenarios) %>%
  filter(scenario %in% scenarios) %>%
  mutate(scenario = ifelse(str_detect(scenario, "_"), str_extract(scenario, "[^_]*$"), scenario)) %>%
  check_portfolio_consistency()


# portcheck_portresults_equity_full <- readRDS(file.path(results_path, investorname_equity, "Equity_results_portfolio.rda")) %>%
portcheck_portresults_equity_full <- readRDS(file.path(results_path, investorname_equity, paste0("Equity_results_", calculation_level, ".rda"))) %>%
  select(-c(trajectory_deviation, trajectory_alignment, scen_tech_share, plan_tech_share, scen_sec_emissions_factor, scen_sec_carsten, scen_alloc_wt_sec_prod, scen_sec_prod, plan_sec_emissions_factor, scen_emission_factor, scen_carsten, plan_emission_factor)) %>%
  filter(!is.na(scenario)) %>%
  check_scenario_settings(scenario_selections = scenarios) %>%
  filter(scenario %in% scenarios) %>%
  mutate(scenario = ifelse(str_detect(scenario, "_"), str_extract(scenario, "[^_]*$"), scenario)) %>%
  check_portfolio_consistency()


sector_exposures <- readRDS(file.path(proc_input_path, paste0(project_name, "_overview_portfolio.rda")))

# Load transition scenarios that will be run by the model
transition_scenarios <- readr::read_csv(file.path(data_location, "transition_scenario_input.csv"), col_types = "cnlnllnnnnnnnnnnnn") %>%
  mutate(
    overshoot_method = ifelse(is.na(overshoot_method), FALSE, overshoot_method),
    duration_of_shock = ifelse(overshoot_method, end_year - year_of_shock + 1, duration_of_shock)
  ) %>%
  check_scenario_consistency()

# Load utilization factors power
capacity_factors_power <- read_capacity_factors(file.path(data_location, "capacity_factors_WEO_2017.csv")) %>%
  select(Region, Technology, region_2dii, capacityfactor_WEO_2016) %>%
  rename(technology = Technology) %>%
  filter(!is.na(capacityfactor_WEO_2016), Region == "World" | (technology %in% c("HydroCap", "NuclearCap", "RenewablesCap") & Region == "OECD")) %>%
  distinct(technology, capacityfactor_WEO_2016) %>%
  rename(capacity_factor = capacityfactor_WEO_2016) %>%
  mutate(scenario_geography = "Global")



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
    ald_sector %in% sectors &
      technology %in% technologies &
      scenario_geography == scenario_geography_filter)

# %>% filter(year %in% c(start_year,2020, 2021, 2022, 2023, 2024, 2025, 2030, 2035, 2040))

df_price <- readr::read_csv(file.path(data_location, paste0("prices_data_", price_data_version, ".csv")), col_types = "ncccccncncncnc") %>%
  filter(year >= start_year) %>%
  check_price_consistency()


#############
# Create shock net profits margins dataframe

net_profit_margins <- net_profit_margin_setup(
  net_profit_margin_coal = net_profit_margin_coal,
  net_profit_margin_coalcap = net_profit_margin_coalcap,
  net_profit_margin_electric = net_profit_margin_electric,
  net_profit_margin_gas = net_profit_margin_gas,
  net_profit_margin_gascap = net_profit_margin_gascap,
  net_profit_margin_hybrid = net_profit_margin_hybrid,
  net_profit_margin_ice = net_profit_margin_ice,
  net_profit_margin_nuclearcap = net_profit_margin_nuclearcap,
  net_profit_margin_oil = net_profit_margin_oil,
  net_profit_margin_renewablescap = net_profit_margin_renewablescap,
  net_profit_margin_hydrocap = net_profit_margin_hydrocap,
  net_profit_margin_oilcap = net_profit_margin_oilcap
)

if (identical(calculation_level, "company") & company_exclusion) {
  excluded_companies <- readr::read_csv(
    file.path(data_location, "exclude-companies.csv"),
    col_types = "cc"
  )
}



###############
# Prepare data for stress test model----------------------------------------
###############

nesting_vars <- c(
  "investor_name", "portfolio_name", "equity_market", "ald_sector", "technology",
  "scenario", "allocation", "scenario_geography"
)
if (identical(calculation_level, "company")) {nesting_vars <- c(nesting_vars, "company_name")}

portcheck_portresults_bonds <- portcheck_portresults_bonds_full %>%
  mutate(scenario = str_replace(scenario, "NPSRTS", "NPS")) %>%
  tidyr::complete(
    year = seq(start_year, start_year + 5),
    nesting(!!!syms(nesting_vars))
  ) %>%
  mutate(plan_tech_prod = dplyr::if_else(is.na(plan_tech_prod), 0, plan_tech_prod)) %>%
  apply_filters(
    investor = investorname_bonds,
    sectors = sectors,
    technologies = technologies,
    scenario_geography_filter = scenario_geography_filter,
    scenarios = scenarios_filter,
    allocation_method = allocation_method_equity,
    start_analysis = start_year
  ) %>%
  filter(
    allocation == allocation_method_equity,
    equity_market == equity_market_filter
  ) %>%
  distinct_all()

# check scenario availability across data inputs for bonds
check_scenario_availability(
  portfolio = portcheck_portresults_bonds,
  scen_data = scenario_data,
  scenarios = scenarios_filter
)

portcheck_portresults_equity <- portcheck_portresults_equity_full %>%
  mutate(scenario = str_replace(scenario, "NPSRTS", "NPS")) %>%
  tidyr::complete(
    year = seq(start_year, start_year + 5),
    nesting(!!!syms(nesting_vars))
  ) %>%
  mutate(plan_tech_prod = dplyr::if_else(is.na(plan_tech_prod), 0, plan_tech_prod)) %>%
  apply_filters(
    investor = investorname_equity,
    sectors = sectors,
    technologies = technologies,
    scenario_geography_filter = scenario_geography_filter,
    scenarios = scenarios_filter,
    allocation_method = allocation_method_equity,
    start_analysis = start_year
  ) %>%
  filter(
    allocation == allocation_method_equity,
    equity_market == equity_market_filter
  ) %>%
  distinct_all()

# check scenario availability across data inputs for equity
check_scenario_availability(
  portfolio = portcheck_portresults_equity,
  scen_data = scenario_data,
  scenarios = scenarios_filter
)

equity_port_aum <- sector_exposures %>%
  filter(asset_type == "Equity") %>%
  group_by(investor_name, portfolio_name) %>%
  summarise(
    asset_portfolio_value = sum(valid_value_usd),
    .groups = "drop_last"
  )

bonds_port_aum <- sector_exposures %>%
  group_by(investor_name, portfolio_name) %>%
  filter(asset_type == "Bonds") %>%
  summarise(
    asset_portfolio_value = sum(valid_value_usd),
    .groups = "drop_last"
  )

###############
# Create input data for stress test model----------------------------------------
###############

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

#   portcheck_compresults_bonds <- portcheck_compresults_bonds %>%
#     group_by(investor_name,portfolio_name,ald_sector,technology,year,scenario_geography,scenario,investor_name,portfolio_name,allocation) %>%
#     summarise(plan_alloc_wt_tech_prod = sum(plan_alloc_wt_tech_prod),
#               scen_alloc_wt_tech_prod= sum(scen_alloc_wt_tech_prod),
#               .groups = "drop_last") %>%
#     mutate(id = 'PortfolioLevel')
# }


# Equity results  ------------------------------------------------------------------

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

  # Convert capacity (MW)to generation (MWh) for power sector
  equity_annual_profits <- portcheck_portresults_equity %>%
    convert_cap_to_generation(capacity_factors_power = capacity_factors_power) %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
      start_analysis = start_year,
      end_analysis = end_year
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
      end_year = end_year
    )

  if (exists("excluded_companies")) {
    equity_annual_profits <- equity_annual_profits %>%
      exclude_companies(
        exclusion = excluded_companies,
        scenario_baseline = scenario_to_follow_baseline,
        scenario_ls = scenario_to_follow_ls
      )
  }

  equity_annual_profits <- equity_annual_profits %>%
    join_price_data(df_prices = df_prices) %>%
    join_net_profit_margins(net_profit_margins = net_profit_margins) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate)

  qa_annual_profits_eq <- qa_annual_profits_eq %>%
    bind_rows(
      equity_annual_profits %>%
        mutate(year_of_shock = transition_scenario_i$year_of_shock)
    )

  plan_carsten_equity <- portcheck_portresults_equity %>%
    filter(
      year == start_year,
      technology %in% technologies,
      scenario_geography == scenario_geography_filter
    )

  if (identical(calculation_level, "company")) {
    plan_carsten_equity <- plan_carsten_equity %>%
      distinct(
        investor_name, portfolio_name, company_name, ald_sector, technology,
        scenario_geography, year, plan_carsten, plan_sec_carsten
      )

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

  } else {
    plan_carsten_equity <- plan_carsten_equity %>%
    distinct(
      investor_name, portfolio_name, ald_sector, technology,
      scenario_geography, year, plan_carsten, plan_sec_carsten
    )

    equity_results <- bind_rows(
      equity_results,
      asset_value_at_risk(
        data = equity_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_equity,
        port_aum = equity_port_aum,
        flat_multiplier = 1
      )
    )
  }

}

# Output equity results
equity_results %>% write_results(
  path_to_results = results_path,
  investorname = investorname_equity,
  asset_type = "equity",
  level = calculation_level,
  file_type = "csv"
)

# Corporate bonds results (flat multiplier PRA 0.15) ---------------------------------------------------------

bonds_results <- c()
qa_annual_profits_cb <- c()

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
    mutate(Baseline = NPS) %>% # FIXME this should be parameterized!!
    rename(
      year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
      SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
    ) %>%
    group_by(ald_sector, technology) %>%
    mutate(
      late_sudden_price = late_sudden_prices(
        SDS_price = SDS_price,
        Baseline_price = Baseline_price,
        overshoot_method = overshoot_method
      )
    ) %>%
    ungroup()

  bonds_annual_profits <- portcheck_portresults_bonds %>%
    convert_cap_to_generation(capacity_factors_power = capacity_factors_power) %>%
    extend_scenario_trajectory(
      scenario_data = scenario_data,
      start_analysis = start_year,
      end_analysis = end_year
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
      end_year = end_year
    )

  if (exists("excluded_companies")) {
    bonds_annual_profits <- bonds_annual_profits %>%
      exclude_companies(
        exclusion = excluded_companies,
        scenario_baseline = scenario_to_follow_baseline,
        scenario_ls = scenario_to_follow_ls
      )
  }

  bonds_annual_profits <- bonds_annual_profits %>%
    join_price_data(df_prices = df_prices) %>%
    join_net_profit_margins(net_profit_margins = net_profit_margins) %>%
    calculate_net_profits() %>%
    dcf_model_techlevel(discount_rate = discount_rate)

  qa_annual_profits_cb <- qa_annual_profits_cb %>%
    bind_rows(
      bonds_annual_profits %>%
        mutate(year_of_shock = transition_scenario_i$year_of_shock)
    )

  plan_carsten_bonds <- portcheck_portresults_bonds %>%
    filter(
      year == start_year,
      technology %in% technologies,
      scenario_geography == scenario_geography_filter
    )

  if(identical(calculation_level, "company")) {
    plan_carsten_bonds <- plan_carsten_bonds %>%
      distinct(
        investor_name, portfolio_name, company_name, ald_sector, technology,
        scenario_geography, year, plan_carsten, plan_sec_carsten
      )

    if (!exists("excluded_companies")) {
      bonds_results <- bind_rows(
        bonds_results,
        company_asset_value_at_risk(
          data = equity_annual_profits,
          terminal_value = terminal_value,
          shock_scenario = shock_scenario,
          div_netprofit_prop_coef = div_netprofit_prop_coef,
          plan_carsten = plan_carsten_bonds,
          port_aum = bonds_port_aum,
          flat_multiplier = 0.15,
          exclusion = NULL
        )
      )
    } else {
      bonds_results <- bind_rows(
        bonds_results,
        company_asset_value_at_risk(
          data = equity_annual_profits,
          terminal_value = terminal_value,
          shock_scenario = shock_scenario,
          div_netprofit_prop_coef = div_netprofit_prop_coef,
          plan_carsten = plan_carsten_bonds,
          port_aum = bonds_port_aum,
          flat_multiplier = 0.15,
          exclusion = excluded_companies
        )
      )
    }

  } else {
    plan_carsten_bonds <- plan_carsten_bonds %>%
      distinct(
        investor_name, portfolio_name, ald_sector, technology,
        scenario_geography, year, plan_carsten, plan_sec_carsten
      )

    bonds_results <- bind_rows(
      bonds_results,
      asset_value_at_risk(
        data = equity_annual_profits,
        terminal_value = terminal_value,
        shock_scenario = shock_scenario,
        div_netprofit_prop_coef = div_netprofit_prop_coef,
        plan_carsten = plan_carsten_bonds,
        port_aum = bonds_port_aum,
        flat_multiplier = 0.15
      )
    )
  }

}

# Output bonds results
bonds_results %>% write_results(
  path_to_results = results_path,
  investorname = investorname_bonds,
  asset_type = "bonds",
  level = calculation_level,
  file_type = "csv"
)




#-QA section-----------

# price trajectories
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

prices_over_time <- show_price_trajectories()


# production trajectories
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

production_over_time <- show_prod_trajectories(
  data = scenario_data,
  source = c("ETP2017", "WEO2019"),
  ald_sector = sectors,
  technology = technologies,
  geography_filter = scenario_geography_filter
)


# distribution of shock impact over time by technology

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

technology_impact_by_shock_year_eq <- show_impact_by_shock_year(
  data = equity_results,
  level = "technology"
)
technology_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = bonds_results,
  level = "technology"
)

# distribution of shock impact over time by sector

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_eq <- show_impact_by_shock_year(
  data = equity_results,
  level = "ald_sector"
)
sector_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = bonds_results,
  level = "ald_sector"
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_eq <- show_var_change_by_shock_year(
  data = equity_results,
  level = "technology"
)
technology_change_by_shock_year_cb <- show_var_change_by_shock_year(
  data = bonds_results,
  level = "technology"
)




# comparison of baseline, target and l&s production paths by technology

data_prod_baseline <- qa_annual_profits_eq

if (identical(calculation_level, "company")) {
  data_prod_baseline <- data_prod_baseline %>%
    group_by(year, investor_name, portfolio_name, scenario_geography,
             ald_sector, technology, year_of_shock) %>%
    summarise(
      baseline = sum(baseline, na.rm = TRUE),
      scen_to_follow_aligned = sum(scen_to_follow_aligned, na.rm = TRUE),
      late_sudden = sum(late_sudden, na.rm = TRUE)
    ) %>%
    ungroup()
}

prod_baseline_target_ls <- show_prod_baseline_target_ls_pf(
  data = data_prod_baseline,
  geography_filter = scenario_geography_filter,
  shock_year = 2030
)


# check the value technology share (plan carsten) of each asset type
# in the portfolio
# expectation: In sum, these should be well below 1, but must be greater than 0

tech_share_eq <- show_pf_technology_shares(data = plan_carsten_equity)

tech_share_cb <- show_pf_technology_shares(data = plan_carsten_bonds)


# Check if carbon budgets are met for all technologies

# yearly

carbon_budgets_eq <- qa_annual_profits_eq %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = FALSE
  )

carbon_budgets_cb <- qa_annual_profits_cb %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = FALSE
  )

# overall

sum_carbon_budgets_eq <- qa_annual_profits_eq %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = TRUE
  )

sum_carbon_budgets_cb <- qa_annual_profits_cb %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = TRUE
  )
