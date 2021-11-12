source(file.path("R", "functions.R"))

function_paths <- c(
  "0_global_functions_st.R",
  file.path(
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
)

source_all(function_paths)

inputs_path <- get_st_data_path()
results_path <- get_st_data_path("ST_PROJECT_FOLDER_OUTPUT")
graph_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER_OUTPUT"), "graphs")
if (!fs::dir_exists(graph_path)) {fs::dir_create(graph_path)}

cfg_st <- config::get(file = "st_project_settings.yml")
project_name <- cfg_st$project_name

# price trajectories
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

# production trajectories
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
) +
ggplot2::theme(
  axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
  legend.background = ggplot2::element_rect(fill = "white"),
  legend.position = "bottom",
  panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
  panel.grid.major.y = ggplot2::element_line(colour = "lightgrey"),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  plot.background = ggplot2::element_rect(fill = "white"),
  strip.background = ggplot2::element_rect(fill = "lightgrey")
)

ggplot2::ggsave(
  "production_over_time.png",
  plot = production_over_time,
  path = graph_path
)

# distribution of shock impact over time by technology

loans_results <- readr::read_csv(file.path(results_path, "stress_test_results_loans_comp.csv"))

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

technology_impact_by_shock_year_cl <- show_impact_by_shock_year(
  data = loans_results,
  level = "technology"
)

ggplot2::ggsave(
  "technology_impact_by_shock_year_cl.png",
  plot = technology_impact_by_shock_year_cl,
  path = graph_path
)

# distribution of shock impact over time by sector

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_cl <- show_impact_by_shock_year(
  data = loans_results,
  level = "ald_sector"
)

ggplot2::ggsave(
  "sector_impact_by_shock_year_cl.png",
  plot = sector_impact_by_shock_year_cl,
  path = graph_path
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_cl <- show_var_change_by_shock_year(
  data = loans_results,
  level = "technology"
)

ggplot2::ggsave(
  "technology_change_by_shock_year_cl.png",
  plot = technology_change_by_shock_year_cl,
  path = graph_path
)

# comparison of baseline, target and l&s production paths by technology
# TODO: currently not possibe because input file is unavailable
# pre processing

qa_annual_profits_lbk <- annual_profits %>%
  dplyr::mutate(year_of_shock = transition_scenario$year_of_shock)

qa_annual_profits_lbk_pf <- qa_annual_profits_lbk %>%
  dplyr::group_by(year, investor_name, portfolio_name, scenario_geography,
           ald_sector, technology, year_of_shock) %>%
  dplyr::summarise(
    baseline = sum(baseline, na.rm = TRUE),
    scen_to_follow_aligned = sum(scen_to_follow_aligned, na.rm = TRUE),
    late_sudden = sum(late_sudden, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

prod_baseline_target_ls <- show_prod_baseline_target_ls_pf(
  data = qa_annual_profits_lbk_pf,
  geography_filter = scenario_geography_filter,
  shock_year = 2030
) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major.y = element_line(colour = "lightgrey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "lightgrey")
  )


# check the value technology share (plan carsten) of each asset type
# in the portfolio
# TODO: currently not possibe because input file is unavailable
# pre process: needs aggregation to pf level
plan_carsten_loanbook_pf <- plan_carsten %>%
  dplyr::group_by(year, investor_name, portfolio_name, scenario_geography,
           ald_sector, technology) %>%
  dplyr::summarise(plan_carsten = sum(plan_carsten, na.rm = TRUE)) %>%
  dplyr::ungroup()

# expectation: In sum, these should be well below 1, but must be greater than 0
tech_share_lbk <- show_pf_technology_shares(data = plan_carsten_loanbook_pf)




# Check if carbon budgets are met for all technologies
# TODO: currently not possibe because input file is unavailable
# yearly

carbon_budgets_lbk <- qa_annual_profits_lbk %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = FALSE
  )

# overall

sum_carbon_budgets_lbk <- qa_annual_profits_lbk %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = TRUE
  )



# credit risk QA graphs
loans_expected_loss <- readr::read_csv(file.path(results_path, glue::glue("stress_test_results_lb_comp_el_{project_name}")))
# overall change of credit risk graphs
plot_pd_change_company_tech <- loans_expected_loss %>%
  overall_pd_change_company_sector(
    shock_year = 2030,
    sector_filter = c("Oil&Gas", "Automotive"),
    company_filter = c("canadian natural resources ltd", "honda motor co ltd"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_pd_change_company_sector.png",
  plot = plot_pd_change_company_sector,
  path = graph_path
)

loans_overall_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_lb_sector_pd_changes_overall.csv"))

plot_pd_change_shock_year_tech <- loans_overall_pd_changes_sector %>%
  overall_pd_change_sector_shock_year(
    scenario_filter = c("Carbon balance 2025", "Carbon balance 2030", "Carbon balance 2035"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_pd_change_shock_year_sector.png",
  plot = plot_pd_change_shock_year_sector,
  path = graph_path
)

# annual change of credit risk graphs
# TODO: file does currently not exist
loans_annual_pd_changes <- readr::read_csv(file.path(results_path, "stress_test_results_lb_company_pd_changes_annual.csv"))

plot_annual_pd_change_company_tech <- loans_annual_pd_changes %>%
  annual_pd_change_company_sector(
    shock_year = 2030,
    company_filter = c("canadian natural resources ltd", "mazda motor corp", "honda motor co ltd"),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_annual_pd_change_company_sector.png",
  plot = plot_annual_pd_change_company_sector,
  path = graph_path
)

loans_annual_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_lb_sector_pd_changes_annual.csv"))

plot_annual_pd_change_shock_year_tech <- loans_annual_pd_changes_sector %>%
  annual_pd_change_sector_shock_year(
    shock_year_filter = c(2025, 2030, 2035),
    geography_filter = "Global"
  )

ggplot2::ggsave(
  "plot_annual_pd_change_shock_year_sector.png",
  plot = plot_annual_pd_change_shock_year_sector,
  path = graph_path
)

