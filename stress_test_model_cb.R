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
