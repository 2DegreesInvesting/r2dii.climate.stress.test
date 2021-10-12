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

technology_impact_by_shock_year_eq <- show_impact_by_shock_year(
  data = equity_results,
  level = "technology"
)

# distribution of shock impact over time by sector-----------------------------

# expectation for QA: earlier shock years should be closer to a zero impact
# than later shock years

sector_impact_by_shock_year_eq <- show_impact_by_shock_year(
  data = equity_results,
  level = "ald_sector"
)

# expectation for QA: changes should be monotonous over the shock years

technology_change_by_shock_year_eq <- show_var_change_by_shock_year(
  data = equity_results,
  level = "technology"
)

# comparison of baseline, target and l&s production paths by technology--------

data_prod_baseline <- qa_annual_profits_eq

data_prod_baseline <- data_prod_baseline %>%
  dplyr::group_by(year, investor_name, portfolio_name, scenario_geography,
           ald_sector, technology, year_of_shock) %>%
  dplyr::summarise(
    baseline = sum(baseline, na.rm = TRUE),
    scen_to_follow_aligned = sum(scen_to_follow_aligned, na.rm = TRUE),
    late_sudden = sum(late_sudden, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()


prod_baseline_target_ls <- show_prod_baseline_target_ls_pf(
  data = data_prod_baseline,
  geography_filter = scenario_geography_filter,
  shock_year = 2030
)


# check the value technology share (plan carsten) of each asset type
# in the portfolio
# expectation: In sum, these should be well below 1, but must be greater than 0

tech_share_eq <- show_pf_technology_shares(data = plan_carsten_equity)

# Check if carbon budgets are met for all technologies-------------------------

# ... yearly-------------------------------------------------------------------

carbon_budgets_eq <- qa_annual_profits_eq %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = FALSE
  )

# ... overall------------------------------------------------------------------

sum_carbon_budgets_eq <- qa_annual_profits_eq %>%
  show_carbon_budget(
    scenarios = scenario_data,
    target_scenario = scenario_to_follow_ls,
    scenario_name_qa = "Carbon balance 2030",
    cumulative = TRUE
  )


# credit risk QA graphs--------------------------------------------------------

# ... overall change of credit risk graphs-------------------------------------
plot_pd_change_company_sector <- equity_expected_loss %>%
  overall_pd_change_company_sector(
    shock_year = 2030,
    sector_filter = c("Power", "Automotive"),
    company_filter = c("Daimler Ag", "Enel Spa"),
    geography_filter = scenario_geography_filter
  )

ggplot2::ggsave(
  "plot_pd_change_company_sector.png",
  plot = plot_pd_change_company_sector,
  path = r2dii.utils::path_dropbox_2dii(outputs_path)
)

plot_pd_change_shock_year_sector <- equity_overall_pd_changes_sector %>%
  overall_pd_change_sector_shock_year(
    scenario_filter = c("Carbon balance 2025", "Carbon balance 2030", "Carbon balance 2035"),
    geography_filter = scenario_geography_filter
  )

ggplot2::ggsave(
  "plot_pd_change_shock_year_sector.png",
  plot = plot_pd_change_shock_year_sector,
  path = r2dii.utils::path_dropbox_2dii(outputs_path)
)

# ... annual change of credit risk graphs--------------------------------------
plot_annual_pd_change_company_sector <- equity_annual_pd_changes %>%
  annual_pd_change_company_sector(
    shock_year = 2030,
    company_filter = c("Daimler Ag", "Enel Spa", "Total Sa"),
    geography_filter = scenario_geography_filter
  )

ggplot2::ggsave(
  "plot_annual_pd_change_company_sector.png",
  plot = plot_annual_pd_change_company_sector,
  path = r2dii.utils::path_dropbox_2dii(outputs_path)
)

plot_annual_pd_change_shock_year_sector <- equity_annual_pd_changes_sector %>%
  annual_pd_change_sector_shock_year(
    shock_year_filter = c(2025, 2030, 2035),
    geography_filter = scenario_geography_filter
  )

ggplot2::ggsave(
  "plot_annual_pd_change_shock_year_sector.png",
  plot = plot_annual_pd_change_shock_year_sector,
  path = r2dii.utils::path_dropbox_2dii(outputs_path)
)
