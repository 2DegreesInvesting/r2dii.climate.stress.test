source(file.path("R", "functions.R"))

function_paths <- file.path(
  "R",
  c(
    "annual_pd_change_company_sector.R",
    "annual_pd_change_sector_shock_year.R",
    "get_st_data_path.R",
    "overall_pd_change_company_sector.R",
    "overall_pd_change_sector_shock_year.R",
    "qa_graphs_st.R",
    "stress_test_model_functions.R"
  )
)

source_all(function_paths)

inputs_path <- get_st_data_path()
results_path <- get_st_data_path("ST_PROJECT_FOLDER_OUTPUT")
graph_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER_OUTPUT"), "graphs")
if (!fs::dir_exists(graph_path)) {fs::dir_create(graph_path)}

cfg_st <- config::get(file = "nonstandard/st_project_settings.yml")
project_name <- cfg_st$project_name

# price trajectories-----------------------------------------------------------
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

df_price <- readr::read_csv(file.path(inputs_path, "prices_data_2021Q1.csv"))

prices_over_time <- show_price_trajectories(
  data = df_price,
  price_scenarios = c("NPS", "SDS")
)

ggplot2::ggsave(
  "prices_over_time.png",
  plot = prices_over_time,
  path = graph_path
)

# production trajectories------------------------------------------------------
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

scenario_data <- readr::read_csv(file.path(inputs_path, "Scenarios_AnalysisInput_2020.csv"))

production_over_time <- show_prod_trajectories(
  data = scenario_data,
  source = c("ETP2017", "WEO2019"),
  ald_sector = c("Automotive", "Coal", "Oil&Gas", "Power"),
  technology = c(
    "Electric", "Hybrid", "ICE", "Coal", "Oil", "Gas", "CoalCap", "GasCap",
    "OilCap", "HydroCap", "NuclearCap", "RenewablesCap"
  ),
  geography_filter = "Global"
)

ggplot2::ggsave(
  "production_over_time.png",
  plot = production_over_time,
  path = graph_path
)

# distribution of shock impact over time by technology-------------------------

equity_results <- readr::read_csv(file.path(results_path, "stress_test_results_equity_comp.csv"))

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

technology_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = results,
  level = "technology"
)

ggplot2::ggsave(
  "technology_impact_by_shock_year_cb.png",
  plot = technology_impact_by_shock_year_cb,
  path = graph_path
)

# distribution of shock impact over time by sector-----------------------------

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_cb <- show_impact_by_shock_year(
  data = results,
  level = "ald_sector"
)

ggplot2::ggsave(
  "sector_impact_by_shock_year_cb.png",
  plot = sector_impact_by_shock_year_cb,
  path = graph_path
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_cb <- show_var_change_by_shock_year(
  data = results,
  level = "technology"
)

ggplot2::ggsave(
  "technology_change_by_shock_year_cb.png",
  plot = technology_change_by_shock_year_cb,
  path = graph_path
)

# check the value technology share (plan carsten) of each asset type
# in the portfolio
# expectation: In sum, these should be well below 1, but must be greater than 0
# TODO: currently not possibe because input file is unavailable
tech_share_cb <- show_pf_technology_shares(data = plan_carsten)


# Check if carbon budgets are met for all technologies-------------------------
# TODO: currently not possibe because input file is unavailable
# ... yearly-------------------------------------------------------------------
qa_annual_profits_cb <- annual_profits %>%
  dplyr::mutate(year_of_shock = transition_scenario$year_of_shock)

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
bonds_expected_loss <- readr::read_csv(file.path(results_path, glue::glue("stress_test_results_cb_comp_el_{project_name}")))
# ... overall change of credit risk graphs-------------------------------------
plot_pd_change_company_tech <- bonds_expected_loss %>%
  overall_pd_change_companysector(
    shock_year = 2030,
    sector_filter = c("Power", "Automotive"),
    company_filter = c("Daimler Ag", "Enel Spa"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_pd_change_company_sector.png",
  plot = plot_pd_change_company_sector,
  path = graph_path
)

bonds_overall_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_cb_sector_pd_changes_overall.csv"))

plot_pd_change_shock_year_tech <- bonds_overall_pd_changes_sector %>%
  overall_pd_change_sector_shock_year(
    scenario_filter = c("Carbon balance 2025", "Carbon balance 2030", "Carbon balance 2035"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_pd_change_shock_year_sector.png",
  plot = plot_pd_change_shock_year_sector,
  path = graph_path
)

# ... annual change of credit risk graphs--------------------------------------
# TODO: file does currently not exist
bonds_annual_pd_changes <- readr::read_csv(file.path(results_path, "stress_test_results_cb_company_pd_changes_annual.csv"))

plot_annual_pd_change_company_tech <- bonds_annual_pd_changes %>%
  annual_pd_change_company_sector(
    shock_year = 2030,
    company_filter = c("Daimler Ag", "Enel Spa", "Total Sa"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_annual_pd_change_company_sector.png",
  plot = plot_annual_pd_change_company_sector,
  path = graph_path
)

bonds_annual_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_cb_sector_pd_changes_annual.csv"))

plot_annual_pd_change_shock_year_tech <- bonds_annual_pd_changes_sector %>%
  annual_pd_change_sector_shock_year(
    shock_year_filter = c(2025, 2030, 2035),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_annual_pd_change_shock_year_sector.png",
  plot = plot_annual_pd_change_shock_year_sector,
  path = graph_path
)

