## Project Initialisation

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(purrr)
library(zoo)


source(file.path("R", "get_st_data_path.R"))
source(file.path("R", "utils.R"))
source(file.path("R", "set_paths.R"))
source("stress_test_model_functions.R")
source("0_global_functions_st.R")

################
# INPUT VARIABLES
################

#### Project location----------------------------------------

# Set Project Settings

# within the "st_project_setting.yml" config file, set the project_name,
# the twodii_internal switch and the external data locations, if necessary.
#
# the project_name will determine the name of the folder that is to be used for
# locating input and output directories for this project
#
# Set twodii_internal to TRUE to run the analysis on an internal 2dii laptop
# This setting uses the dropbox connection for data import
# Set twodii_internal to FALSE, tu use external data locations
# Specify these data locations in the config file "st_project_settings.yml"
# in this repo


cfg_st <- config::get(file = "st_project_settings.yml")
check_valid_cfg(cfg = cfg_st, expected_no_args = 5)
project_name <- cfg_st$project_name
twodii_internal <- cfg_st$project_internal$twodii_internal
project_location_ext <- cfg_st$project_internal$project_location_ext

data_location <- file.path(get_st_data_path(), data_path())

# set input path
set_project_paths(
  project_name = project_name,
  twodii_internal = twodii_internal,
  project_location_ext = project_location_ext
)


# THIS NEEDS TO BE INVESTIGATED! PROBABLY LOOP OVER INV OR ALLOW SPECIFICATION IN CONFIG
cfg_litigation_params <- config::get(file = "params_litigation_risk.yml")
# investorname_equity <- cfg_litigation_params$investor_name$investor_name_equity
# investorname_bonds <- cfg_litigation_params$investor_name$investor_name_bonds
# investorname_loanbook <- cfg_litigation_params$investor_name$investor_name_loanbook


#### Analysis Parameters----------------------------------------
# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

cfg <- config::get(
  file = file.path(project_location, "10_Parameter_File", "AnalysisParameters.yml")
)
# OPEN: check_valid_cfg() not applicable here
start_year <- cfg$AnalysisPeriod$Years.Startyear
# TODO: is this being used for anything???
dataprep_timestamp <- cfg$TimeStamps$DataPrep.Timestamp


##### Filters----------------------------------------
# The filter settings should mirror those from the parent PACTA project by default
# There may still be cases of certain sectors or geographies that work in PACTA
# but not yet in stress testing
# move to config once mechanism to include/exclude filters from original pacta project exists

#### OPEN: This could largely be taken from cfg file. No apparent reason why not.
scenario_geography_filter <- cfg_litigation_params$lists$scenario_geography_list
# scenario_geography_filter <- cfg$Lists$Scenario.Geography.List

# ALLOW ONLY precisely the scenarios that are supposed to be kept from the portfolio and scen_data
# NOTE scenarios from the same source, same secenario name and diff years will likely fail
# E.g. WEO2019_SDS AND WEO2020_SDS will produce near-duplicates that break the analysis
scenarios <- c(
  "B2DS",
  "CPS",
  "NPS",
  "NPSRTS",
  "SDS",
  # "ETP2017_B2DS",
  # "ETP2017_NPS",
  # "ETP2017_SDS",
  # "GECO2019_1.5c",
  # "GECO2019_2c_m",
  # "GECO2019_ref",
  # "WEO2019_CPS",
  # "WEO2019_NPS",
  # "WEO2019_SDS",
  # "WEO2020_NPS",
  # "WEO2020_SDS"
)
# scenarios <- cfg$Large.Universe.Filter$SCENARIO.FILTER

allocation_method <- cfg_litigation_params$allocation_method
equity_market_filter <- cfg$Lists$Equity.Market.List

sectors <- cfg_litigation_params$large_universe_filter$sector_filter
# setors <- cfg$Large.Universe.Filter$SECTOR.FILTER

technologies <- cfg_litigation_params$lists$technology_list
# technologies <- cfg$Lists$Technology.List

#-load required data--------------------------

litigation_risk_scenarios <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("litigation_risk_scenarios.csv")
  ),
  col_types = "ccdddd"
)

company_emissions_data_input <- read_csv(
  file.path(
    project_location,
    "30_Processed_Inputs",
    "litigation_risk_dummy_input.csv"
  ),
  col_types = "dcdcdddddccd"
)

company_ebit_data_input <- read_csv(
  file.path(
    project_location,
    "30_Processed_Inputs",
    "litigation_risk_company_ebit.csv"
  ),
  col_types = "dcc"
)

carbon_delta_plus_damages <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("delta_carbon_budget.csv")
  ),
  col_types = "ccdddd"
)

company_historical_emissions <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("historical_emissions_heede_2017.csv")
  ),
  col_types = "cdddd"
) %>%
  dplyr::mutate(
    dplyr::across(c(scope_1, scope_3, scope_1_plus_3), ~ .x * 1000000)
  )


company_data <- company_emissions_data_input %>%
  mutate(
    scenario = case_when(
      scenario == "2DS" ~ "SDS",
      scenario == "RTS" ~ "NPS",
      TRUE ~ scenario
    )
  ) %>%
  select(-ebit) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(allowed_emission, allowed_production)
  ) %>%
  left_join(
    company_ebit_data_input,
    by = "company_name"
  ) %>%
  filter(
    ebit > 0
  )
# TODO: There are still duplicates because some companies are in multiple sectors


#-Company Level Calculation-----------------------------

company_data_overshoot <- company_data %>%
  mutate(
    actual_emissions = current_production * avg_ef,
    allowed_emission_b2ds = allowed_production_B2DS * avg_ef,
    allowed_emission_sds = allowed_production_SDS * avg_ef,
    allowed_emission_cps = allowed_production_CPS * avg_ef,
    overshoot_actual_b2ds = actual_emissions - allowed_emission_b2ds,
    overshoot_actual_sds = actual_emissions - allowed_emission_sds,
    overshoot_actual_cps = actual_emissions - allowed_emission_cps
  )


company_carbon_budgets <- company_data_overshoot %>%
  mutate(
    delta_allowed_b2ds_sds = allowed_emission_sds - allowed_emission_b2ds,
    delta_allowed_sds_cps = allowed_emission_cps - allowed_emission_sds,
    delta_allowed_b2ds_cps = allowed_emission_cps - allowed_emission_b2ds,
    overshoot_delta_b2ds_sds = case_when(
      overshoot_actual_b2ds < 0 ~ 0,
      overshoot_actual_b2ds > delta_allowed_b2ds_sds ~ delta_allowed_b2ds_sds,
      TRUE ~ overshoot_actual_b2ds
    ),
    overshoot_delta_b2ds_cps = case_when(
      overshoot_actual_b2ds < 0 ~ 0,
      overshoot_actual_b2ds > delta_allowed_b2ds_cps ~ delta_allowed_b2ds_cps,
      TRUE ~ overshoot_actual_b2ds
    ),
    overshoot_delta_sds_cps = case_when(
      overshoot_actual_sds < 0 ~ 0,
      overshoot_actual_sds > delta_allowed_sds_cps ~ delta_allowed_sds_cps,
      TRUE ~ overshoot_actual_sds
    )
  )


# Carbon Delta Damages (CDD) liability model

scenario <- litigation_risk_scenarios %>% filter(model == "CDD")

chance_within_target <- cfg_litigation_params$litigation$chance_within_target

cdd_carbon_budget_plus_damages <- carbon_delta_plus_damages %>%
  filter(likelihood_stay_within_target == chance_within_target)

delta_carbon_budget_b2ds_sds <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario == "SDS") %>%
  pull(delta_carbon_budget)

delta_carbon_budget_b2ds_cps <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario %in% c("SDS", "CPS")) %>%
  summarise(delta_carbon_budget = sum(delta_carbon_budget, na.rm = TRUE)) %>%
  pull()

delta_carbon_budget_sds_cps <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario == "CPS") %>%
  pull(delta_carbon_budget)

cdd_company_carbon_budgets <- company_carbon_budgets %>%
  mutate(
    contribution_sds_b2ds = .data$overshoot_delta_b2ds_sds / delta_carbon_budget_b2ds_sds,
    contribution_cps_b2ds = overshoot_delta_b2ds_cps / delta_carbon_budget_b2ds_cps,
    contribution_cps_sds = overshoot_delta_sds_cps / delta_carbon_budget_sds_cps
  )

delta_carbon_damage_b2ds_sds <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario == "SDS") %>%
  pull(delta_econ_damages_usd)

delta_carbon_damage_b2ds_cps <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario %in% c("SDS", "CPS")) %>%
  summarise(delta_econ_damages_usd = sum(delta_econ_damages_usd, na.rm = TRUE)) %>%
  pull()

delta_carbon_damage_sds_cps <- cdd_carbon_budget_plus_damages %>%
  filter(target_scenario == "CPS") %>%
  pull(delta_econ_damages_usd)

# TODO: initialise using a list or df with correct number of entries
cdd_results <- c()

for (i in seq(1, nrow(scenario))) {
  cdd_scenario_i <- scenario[i, ]

  cdd_company_i <- cdd_company_carbon_budgets %>%
    mutate(
      scenario_name = cdd_scenario_i$litigation_scenario,
      damage_sds_b2ds = contribution_sds_b2ds * delta_carbon_damage_b2ds_sds,
      damage_cps_b2ds = contribution_cps_b2ds * delta_carbon_damage_b2ds_cps,
      damage_cps_sds = contribution_cps_sds * delta_carbon_damage_sds_cps,
      # TODO: make scenarios more easily selectable
      cdd_liability_total = damage_cps_b2ds * scenario$exp_share_damages_paid,
      cdd_liability = cdd_liability_total /
        cdd_scenario_i$timeframe_emissions_overshoot,
      cdd_liability_perc_ebit = cdd_liability / ebit
    ) %>%
    select(
      scenario_name, Bloomberg_ID, company_name,
      type, unit_emissions, actual_emissions,
      allowed_emission_b2ds, allowed_emission_sds, allowed_emission_cps,
      overshoot_delta_b2ds_sds, overshoot_delta_sds_cps, overshoot_delta_b2ds_cps,
      contribution_sds_b2ds, contribution_cps_sds, contribution_cps_b2ds,
      damage_sds_b2ds, damage_cps_sds, damage_cps_b2ds,
      cdd_liability, cdd_liability_total, ebit, cdd_liability_perc_ebit
    )

  cdd_results <- cdd_results %>%
    bind_rows(cdd_company_i)
}

cdd_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_cdd.csv")
)



# Social Cost of Carbon (SCC) liability model

scenario <- litigation_risk_scenarios %>% filter(model == "SCC")

# TODO: initialise using a list or df with correct number of entries
scc_results <- c()

for (i in seq(1, nrow(scenario))) {
  scc_scenario_i <- scenario[i, ]

  scc_company_i <- company_carbon_budgets %>%
    mutate(
      scenario_name = scc_scenario_i$litigation_scenario,
      # TODO: make scenario more easily selectable
      overshoot_actual_b2ds = overshoot_delta_b2ds_cps
    ) %>%
    mutate(
      scc_liability_total =
        overshoot_actual_b2ds * scc_scenario_i$scc *
          scc_scenario_i$exp_share_damages_paid,
      scc_liability = scc_liability_total /
        scc_scenario_i$timeframe_emissions_overshoot,
      scc_liability_perc_ebit = scc_liability / ebit
    ) %>%
    select(
      scenario_name, Bloomberg_ID, company_name,
      type, unit_emissions, actual_emissions,
      allowed_emission_b2ds, allowed_emission_sds, allowed_emission_cps,
      overshoot_actual_b2ds, overshoot_actual_sds, overshoot_actual_cps,
      scc_liability, scc_liability_total, ebit, scc_liability_perc_ebit
    )

  scc_results <- scc_results %>%
    bind_rows(scc_company_i)
}

scc_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_scc.csv")
)


# Historical Emissions Responsibility (HER) model

scenario <- litigation_risk_scenarios %>% filter(model == "HER")

backwards_years <- cfg_litigation_params$litigation$backwards_years

# TODO: initialise using a list or df with correct number of entries
her_results <- c()

for (i in seq(1, nrow(scenario))) {
  her_scenario_i <- scenario[i, ]

  if (her_scenario_i$scc > 0 & her_scenario_i$past_yearly_costs_usd == 0) {
    her_company_i <- company_historical_emissions %>%
      left_join(company_ebit_data_input, by = c("company_name")) %>%
      filter(ebit > 0)

    her_company_i <- her_company_i %>%
      mutate(
        scenario_name = her_scenario_i$litigation_scenario,
        her_liability = .data$scope_1_plus_3 *
          her_scenario_i$scc *
          her_scenario_i$exp_share_damages_paid,
        her_liability_total = her_liability,
        her_liability_perc_ebit = her_liability / ebit
      ) %>%
      select(
        scenario_name, company_name, type, # unit_emissions,
        actual_emissions = scope_1_plus_3, share_global_industrial_ghg,
        her_liability, her_liability_total, ebit, her_liability_perc_ebit
      )

    her_results <- her_results %>%
      bind_rows(her_company_i)
  } else if (her_scenario_i$scc == 0 & her_scenario_i$past_yearly_costs_usd > 0) {
    her_company_i <- company_historical_emissions %>%
      left_join(company_ebit_data_input, by = c("company_name")) %>%
      filter(ebit > 0)

    her_company_i <- her_company_i %>%
      mutate(
        scenario_name = her_scenario_i$litigation_scenario,
        her_liability = .data$share_global_industrial_ghg *
          her_scenario_i$past_yearly_costs_usd *
          backwards_years *
          her_scenario_i$exp_share_damages_paid,
        her_liability_total = her_liability,
        # TODO: percentage loss in HER model with total liability, others annual. Why?
        her_liability_perc_ebit = her_liability / ebit
      ) %>%
      select(
        scenario_name, company_name, type, # unit_emissions,
        actual_emissions = scope_1_plus_3, share_global_industrial_ghg,
        her_liability, her_liability_total, ebit, her_liability_perc_ebit
      )

    her_results <- her_results %>%
      bind_rows(her_company_i)
  } else {
    next("Cannot process the scenario parameters for the HER scenario")
  }
}

her_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_her.csv")
)


#######



company_results <- cdd_results %>%
  select(
    scenario_name, company_name,
    # TODO: get proper sectors from PACTA, this is temporary
    sector = type,
    liability = cdd_liability,
    liability_total = cdd_liability_total,
    ebit,
    liability_perc_ebit = cdd_liability_perc_ebit
  ) %>%
  bind_rows(
    scc_results %>%
      select(
        scenario_name, company_name,
        # TODO: get proper sectors from PACTA, this is temporary
        sector = type,
        liability = scc_liability,
        liability_total = scc_liability_total,
        ebit,
        liability_perc_ebit = scc_liability_perc_ebit
      )
  ) %>%
  bind_rows(
    her_results %>%
      select(
        scenario_name, company_name,
        # TODO: get proper sectors from PACTA, this is temporary
        sector = type,
        liability = her_liability,
        liability_total = her_liability_total,
        ebit,
        liability_perc_ebit = her_liability_perc_ebit
      )
  ) %>%
  # TODO: get proper sectors from PACTA, this is temporary
  mutate(
    sector = case_when(
      sector == "O" ~ "Oil&Gas",
      sector == "P" ~ "Power",
      TRUE ~ "Other"
    )
  )

# TODO: for company level impact, the sectors/types need to by summed by comp
company_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company.csv")
)


#-Impact on share price----------------------------

start_year <- cfg_litigation_params$litigation$start_year
end_year <- cfg_litigation_params$litigation$end_year
years_to_litigation_event <- cfg_litigation_params$litigation$years_to_litigation_event
settlement_factor <- cfg_litigation_params$litigation$settlement_factor
growth_rate <- cfg_litigation_params$litigation$growth_rate
discount_rate <- cfg_litigation_params$litigation$discount_rate
terminal_value <- cfg_litigation_params$litigation$percentage_in_terminal_value

# calculate DCF

# TODO: what about these parameters from excel sheet:
# percentage_of_revenue_in_transition_sectors
# gross_profit_margin
# operating_profit_margin
# net_profit_margin
# Scenario assumption (either 6,4, or 2)
company_results <- company_results %>%
  mutate(
    years_to_litigation = years_to_litigation_event,
    settlement_factor = settlement_factor,
    # TODO: how o calculate this for HER model? liability should not increase with years to litigation?
    settlement = liability * years_to_litigation_event * settlement_factor
  )

df_timeframe <- tibble(
  timeframe = seq(start_year, end_year),
  value = NA_real_
) %>%
  pivot_wider(names_from = timeframe, values_from = value)


# TODO: use calculated target profits from transition risk module after settlement
company_results_dcf <- company_results %>%
  bind_cols(
    df_timeframe
  ) %>%
  pivot_longer(
    cols = -c(
      scenario_name, company_name, sector, liability,
      ebit, liability_perc_ebit, years_to_litigation,
      settlement_factor, settlement
    ),
    names_to = "year",
    values_to = "dividends"
  ) %>%
  arrange(scenario_name, company_name, sector, year) %>%
  group_by(scenario_name, company_name, sector) %>%
  mutate(
    t_calc = seq(0, (n() - 1)),
    dividends = ebit * (1 + growth_rate)^t_calc,
    dividends_litigation = dplyr::if_else(
      year == start_year + years_to_litigation_event,
      dividends - settlement,
      dividends
    )
  ) %>%
  ungroup()

reset_post_settlement <- cfg_litigation_params$litigation$reset_post_settlement

if (reset_post_settlement == "start") {
  company_results_dcf <- company_results_dcf %>%
    group_by(scenario_name, company_name, sector) %>%
    mutate(
      dividends_litigation = dplyr::if_else(
        .data$year == start_year + years_to_litigation_event + 1 &
          .data$liability > 0,
        .data$ebit,
        .data$dividends_litigation
      )
    ) %>%
    mutate(
      dividends_litigation = dplyr::if_else(
        .data$year > start_year + years_to_litigation_event + 1 &
          .data$liability > 0,
        .data$ebit *
          (1 + growth_rate)^
            dplyr::lag(.data$t_calc, n = years_to_litigation_event + 1),
        .data$dividends_litigation
      )
    ) %>%
    ungroup()
}

company_results_dcf <- company_results_dcf %>%
  group_by(scenario_name, company_name, sector) %>%
  mutate(
    discounted_dividends = dividends / ((1 + discount_rate)^t_calc),
    discounted_dividends_litigation = dividends_litigation / ((1 + discount_rate)^t_calc)
  ) %>%
  ungroup()



# calculate NPV

company_results_npv <- company_results_dcf %>%
  group_by(scenario_name, company_name, sector) %>%
  summarise(
    npv_baseline = sum(discounted_dividends, na.rm = TRUE) * (1 + terminal_value),
    npv_litigation = sum(discounted_dividends_litigation, na.rm = TRUE) * (1 + terminal_value),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    percentage_value_change = round((npv_litigation - npv_baseline) / npv_baseline, 7)
  )

company_results_npv %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_value_change.csv")
)


#-Portfolio Level Calculation----------------------

# technology shares entirely made up for illustration purposes
# TODO: get these from corresponding pacta project
technology_share_comp <- readRDS(
  testthat::test_path("test_data", "test_pf_impact_litigation.rds")
)


# sector exposures entirely made up for illustration purposes
# TODO: get these from corresponding pacta project
sector_exposures <- readRDS(
  testthat::test_path("test_data", "test_pf_sector_exposures.rds")
)

# portfolio aum for equity, value in USD
port_aum_equity <- sector_exposures %>%
  summarise(port_aum_equity = sum(valid_value_usd, na.rm = TRUE)) %>%
  pull()

technology_exposure <- technology_share_comp %>%
  mutate(
    portfolio_aum_eq = port_aum_equity,
    company_tech_exposure = portfolio_aum_eq * plan_carsten
  ) %>%
  select(-c(year, plan_sec_carsten))

# TODO: merge portfolio comp-tech exposures with company results (percentage loss)
portfolio_liabilities <- company_results_npv %>%
  left_join(
    technology_exposure,
    by = c("company_name", "sector" = "ald_sector")
  ) %>%
  mutate(
    value_change = company_tech_exposure * percentage_value_change
  )

portfolio_liabilities %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_portfolio_impact.csv")
)



### visualization

viz_company_results <- company_results %>%
  filter(.data$liability_perc_ebit >= 0) %>%
  group_by(.data$company_name) %>%
  mutate(max_liability_perc = max(.data$liability_perc_ebit, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(company_name = forcats::fct_reorder(.data$company_name, desc(max_liability_perc)))

comp_graph <- viz_company_results %>%
  ggplot(
    aes(
      x = .data$company_name,
      y = .data$liability_perc_ebit
    )
  ) +
  geom_boxplot(alpha = 0.5) +
  geom_point(aes(color = .data$scenario_name), alpha = 0.5) +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

# subset of companies as in paper
# TODO: cannot replicate the logic for subsetting the companies at the moment
subset_companies <- c(
  "Repsol Sa",
  "Total Sa",
  "Bp Plc",
  "Eni Spa",
  "Chevron Corp",
  "Exxon Mobil Corp",
  "Hess Corp",
  "Royal Dutch Shell Plc",
  "Conocophillips",
  "Marathon Oil Corp",
  "Equinor Asa",
  "Devon Energy Corp",
  "Rwe Ag",
  "Bhp Group Ltd",
  "Eog Resources Inc",
  "Glencore Plc",
  "Canadian Natural Resources Ltd"
)

subset_models <- c(
  "CDD_TMS",
  "HER_CDD_TMS",
  "SCC_40_TMS"
)

viz_company_results_subset <- viz_company_results %>%
  filter(
    company_name %in% subset_companies &
      scenario_name %in% subset_models
  ) %>%
  group_by(.data$company_name) %>%
  mutate(max_liability_perc = max(.data$liability_perc_ebit, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(company_name = forcats::fct_reorder(.data$company_name, desc(max_liability_perc)))

comp_graph_subset <- viz_company_results_subset %>%
  ggplot(
    aes(
      x = .data$company_name,
      y = .data$liability_perc_ebit
    )
  ) +
  geom_boxplot(alpha = 0.5) +
  geom_point(aes(color = .data$scenario_name), alpha = 0.5) +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

viz_company_results_payout_subset <- viz_company_results_subset %>%
  group_by(.data$company_name) %>%
  mutate(max_liability = max(.data$liability_total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(company_name = forcats::fct_reorder(.data$company_name, desc(max_liability)))


comp_graph_payout_subset <- viz_company_results_payout_subset %>%
  ggplot(
    aes(
      x = .data$company_name,
      y = .data$liability_total
    )
  ) +
  geom_col(aes(fill = .data$scenario_name), position = "dodge", alpha = 0.5) +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )
