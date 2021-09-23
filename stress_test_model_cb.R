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
