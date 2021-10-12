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

# price trajectories
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

prices_over_time <- show_price_trajectories()


# production trajectories
# expectation for QA: trajectories should be monotonous, no sudden jumps
# for scenarios in use

production_over_time <- show_prod_trajectories(
  data = scenario_data %>% filter(scenario %in% scenarios_filter),
  source = c("ETP2017", "WEO2019"),
  ald_sector = sectors,
  technology = technologies,
  geography_filter = scenario_geography_filter
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

# distribution of shock impact over time by technology

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

technology_impact_by_shock_year_lbk <- show_impact_by_shock_year(
  data = loanbook_results_pf,
  level = "technology"
)

# distribution of shock impact over time by sector

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_eq <- show_impact_by_shock_year(
  data = loanbook_results_pf,
  level = "ald_sector"
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_lbk <- show_var_change_by_shock_year(
  data = loanbook_results_pf,
  level = "technology"
)


# comparison of baseline, target and l&s production paths by technology
#
# pre processing
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
#
# pre process: needs aggregation to pf level
plan_carsten_loanbook_pf <- plan_carsten_loanbook %>%
  dplyr::group_by(year, investor_name, portfolio_name, scenario_geography,
           ald_sector, technology) %>%
  dplyr::summarise(plan_carsten = sum(plan_carsten, na.rm = TRUE)) %>%
  dplyr::ungroup()

# expectation: In sum, these should be well below 1, but must be greater than 0
tech_share_lbk <- show_pf_technology_shares(data = plan_carsten_loanbook_pf)




# Check if carbon budgets are met for all technologies

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

# overall change of credit risk graphs
plot_pd_change_company_tech <- loanbook_expected_loss %>%
  overall_pd_change_company_technology(
    shock_year = 2030,
    sector_filter = c("Oil&Gas", "Automotive"),
    company_filter = c("canadian natural resources ltd", "honda motor co ltd"),
    geography_filter = "Global"
  )

plot_pd_change_shock_year_tech <- loanbook_overall_pd_changes_sector %>%
  overall_pd_change_technology_shock_year(
    scenario_filter = c("Carbon balance 2025", "Carbon balance 2030", "Carbon balance 2035"),
    geography_filter = "Global"
  )

# annual change of credit risk graphs
plot_annual_pd_change_company_tech <- loanbook_annual_pd_changes %>%
  annual_pd_change_company_technology(
    shock_year = 2030,
    company_filter = c("canadian natural resources ltd", "mazda motor corp", "honda motor co ltd"),
    geography_filter = "Global"
  )

plot_annual_pd_change_shock_year_tech <- loanbook_annual_pd_changes_sector %>%
  annual_pd_change_technology_shock_year(
    shock_year_filter = c(2025, 2030, 2035),
    geography_filter = "Global"
  )

