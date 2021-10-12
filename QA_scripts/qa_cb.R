source(file.path("R", "functions.R"))

function_paths <- c(
  "stress_test_model_functions.R",
  "0_global_functions_st.R",
  file.path(
    "R",
    c(
      "annual_pd_change_company_technology.R",
      "annual_pd_change_technology_shock_year.R",
      "overall_pd_change_company_technology.R",
      "overall_pd_change_technology_shock_year.R",
      "qa_graphs_st.R"
    )
  )
)

source_all(function_paths)

# price trajectories-----------------------------------------------------------
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

prices_over_time <- show_price_trajectories()


# production trajectories------------------------------------------------------
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

production_over_time <- show_prod_trajectories(
  data = scenario_data,
  source = c("ETP2017", "WEO2019"),
  ald_sector = sectors,
  technology = technologies,
  geography_filter = scenario_geography_filter
)


# distribution of shock impact over time by technology-------------------------

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

technology_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = bonds_results,
  level = "technology"
)

# distribution of shock impact over time by sector-----------------------------

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = bonds_results,
  level = "ald_sector"
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_cb <- show_var_change_by_shock_year(
  data = bonds_results,
  level = "technology"
)

# check the value technology share (plan carsten) of each asset type
# in the portfolio
# expectation: In sum, these should be well below 1, but must be greater than 0

tech_share_cb <- show_pf_technology_shares(data = plan_carsten_bonds)


# Check if carbon budgets are met for all technologies-------------------------

# ... yearly-------------------------------------------------------------------

carbon_budgets_cb <- qa_annual_profits_cb %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = FALSE
  )

# ... overall------------------------------------------------------------------

sum_carbon_budgets_cb <- qa_annual_profits_cb %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = TRUE
  )


# credit risk QA graphs--------------------------------------------------------

# ... overall change of credit risk graphs-------------------------------------
plot_pd_change_company_tech <- bonds_expected_loss %>%
  overall_pd_change_company_technology(
    shock_year = 2030,
    sector_filter = c("Power", "Automotive"),
    company_filter = c("Daimler Ag", "Enel Spa"),
    geography_filter = scenario_geography_filter
  )

plot_pd_change_shock_year_tech <- bonds_overall_pd_changes_sector %>%
  overall_pd_change_technology_shock_year(
    scenario_filter = c("Carbon balance 2025", "Carbon balance 2030", "Carbon balance 2035"),
    geography_filter = scenario_geography_filter
  )

# ... annual change of credit risk graphs--------------------------------------
plot_annual_pd_change_company_tech <- bonds_annual_pd_changes %>%
  annual_pd_change_company_technology(
    shock_year = 2030,
    company_filter = c("Daimler Ag", "Enel Spa", "Total Sa"),
    geography_filter = scenario_geography_filter
  )

plot_annual_pd_change_shock_year_tech <- bonds_annual_pd_changes_sector %>%
  annual_pd_change_technology_shock_year(
    shock_year_filter = c(2025, 2030, 2035),
    geography_filter = scenario_geography_filter
  )
