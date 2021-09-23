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
      "calculate_overall_pd_changes.R",
      "create_empty_result_df_pd_changes.R",
      "company_asset_value_at_risk.R",
      "company_expected_loss.R",
      "convert_cap_to_generation.R",
      "exclude_companies.R",
      "extend_scenario_trajectory.R",
      "get_st_data_path.R",
      "interpolate_automotive_scenario.R",
      "overall_pd_change_company_technology.R",
      "overall_pd_change_technology_shock_year.R",
      "qa_graphs_st.R",
      "read_capacity_factors.R",
      "read_company_data.R",
      "read_ngfs_carbon_tax.R",
      "read_pacta_results.R",
      "read_price_data.R",
      "read_transition_scenarios.R",
      "set_paths.R",
      "set_tech_trajectories.R",
      "show_carbon_budget.R",
      "utils.R",
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


# THIS NEEDS TO BE INVESTIGATED! PROBABLY LOOP OVER INV OR ALLOW SPECIFICATION IN CONFIG
investorname_bonds <- "Meta Investor"
investorname_equity <- "Meta Investor"

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

# OPEN: This could largely be taken from cfg file. No apparent reason why not.
scenario_geography_filter <- "Global"
# scenario_geography_filter <- cfg$Lists$Scenario.Geography.List

# ALLOW ONLY precisely the scenarios that are supposed to be kept from the portfolio and scen_data
# NOTE scenarios from the same source, same secenario name and diff years will likely fail
# E.g. WEO2019_SDS AND WEO2020_SDS will produce near-duplicates that break the analysis
scenarios <- c(
  # "B2DS",
  # "CPS",
  # "NPS",
  # "NPSRTS",
  # "SDS"#,
  # "ETP2017_B2DS",
  "ETP2017_NPS",
  "ETP2017_SDS",
  # "WEO2019_CPS",
  "WEO2019_NPS",
  "WEO2019_SDS" # ,
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
risk_free_rate <- cfg_mod$financials$risk_free_rate
lgd_senior_claims <- cfg_mod$financials$lgd_senior_claims
lgd_subordinated_claims <- cfg_mod$financials$lgd_subordinated_claims

###########################################################################
# Load input datasets------------------------------------------------------
###########################################################################

# Load company financial and production data-----------------------------------
# ... get file paths for stresstest masterdata --------------------------------
stresstest_masterdata_files <- create_stressdata_masterdata_file_paths(
  data_prep_timestamp = cfg$TimeStamps$DataPrep.Timestamp,
  twodii_internal = twodii_internal
)

# ... for bonds----------------------------------------------------------------
financial_data_bonds <- read_company_data(path = stresstest_masterdata_files$bonds,
                                          asset_type = "bonds")


# Load PACTA results / bonds portfolio------------------------
bonds_path <- file.path(results_path, investorname_bonds, paste0("Bonds_results_", calculation_level, ".rda"))

pacta_bonds_results_full <- read_pacta_results(
  path = bonds_path,
  asset_type = "bonds",
  level = calculation_level
)

pacta_bonds_results_full <- pacta_bonds_results_full %>%
  dplyr::filter(!is.na(.data$scenario)) %>%
  check_scenario_settings(scenario_selections = scenarios) %>%
  dplyr::filter(.data$scenario %in% .env$scenarios) %>%
  # TODO: temporary fix, remove once all scenario data is used from scenario file
  filter(!(str_detect(.data$scenario, "ETP") & .data$ald_sector == "Power")) %>%
  dplyr::mutate(
    scenario = dplyr::if_else(
      stringr::str_detect(.data$scenario, "_"),
      stringr::str_extract(.data$scenario, "[^_]*$"),
      .data$scenario
    )
  ) %>%
  check_portfolio_consistency()

# TODO: temporary addition, needs to come directly from input
pacta_bonds_results_full <- pacta_bonds_results_full %>%
  group_by(company_name) %>%
  mutate(
    term = round(runif(n = 1, min = 1, max = 10), 0)
  ) %>%
  ungroup()

