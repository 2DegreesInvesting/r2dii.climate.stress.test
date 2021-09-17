## Project Initialisation

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(r2dii.utils)
library(stringr)
library(tibble)
library(tidyr)
library(purrr)
library(zoo)


source(file.path("R","get_st_data_path.R"))
source(file.path("R","utils.R"))
source(file.path("R","set_paths.R"))
source("stress_test_model_functions.R")
source("0_global_functions_st.R")

################
# INPUT VARIABLES
################

############################################################################
#### Project location-------------------------------------------------------
############################################################################

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

############################################################################
#### Analysis Parameters----------------------------------------------------
############################################################################

# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

cfg <- config::get(
  file = file.path(project_location, "10_Parameter_File","AnalysisParameters.yml")
)

path_db_analysis_inputs <- fs::path(
  r2dii.utils::dbox_port_00("07_AnalysisInputs", cfg$TimeStamps$DataPrep.Timestamp)
)

# Get litigation params

# THIS NEEDS TO BE INVESTIGATED! PROBABLY LOOP OVER INV OR ALLOW SPECIFICATION IN CONFIG
cfg_litigation_params <- config::get(file = "params_litigation_risk.yml")
# ADO 1540 - set variables for reading company level PACTA results
investor_name_equity <- cfg_litigation_params$investor_name$investor_name_equity
investor_name_bonds <- cfg_litigation_params$investor_name$investor_name_bonds
flat_multiplier <- cfg_litigation_params$litigation$flat_multiplier

############################################################################
##### Filters---------------------------------------------------------------
############################################################################

# The filter settings should mirror those from the parent PACTA project by default
# There may still be cases of certain sectors or geographies that work in PACTA
# but not yet in stress testing
# move to config once mechanism to include/exclude filters from original pacta project exists

# ADO 1540 - use for filters
start_year <- cfg$AnalysisPeriod$Years.Startyear # TODO: check this is in line with webtool results. seems wrong
horizon <- cfg$AnalysisPeriod$Years.Horizon

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
  #"NPSRTS",
  "SDS"#,
  # "ETP2017_B2DS",
  # "ETP2017_NPS",
  # "ETP2017_SDS",
  # "WEO2019_CPS",
  # "WEO2019_NPS",
  # "WEO2019_SDS" # ,
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

############################################################################
# load required data--------------------------
############################################################################


############################################################################
#### load and prepare litigation risk scenarios-----------------------------
############################################################################
litigation_risk_scenarios <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("litigation_risk_scenarios.csv")
  ),
  col_types = "ccdddd"
)

############################################################################
#### load and prepare company emissions data--------------------------------
############################################################################

# ADO 1540 - read company emissions from PACTA results
company_emissions_data_input_raw_eq <- readRDS(
  file.path(
    results_path, investor_name_equity, "Equity_results_company.rda"
  )
)

company_emissions_data_input_raw_eq <- company_emissions_data_input_raw_eq %>%
  dplyr::mutate(asset_type = "Equity")

company_emissions_data_input_raw_cb <- readRDS(
  file.path(
    results_path, investor_name_bonds, "Bonds_results_company.rda"
  )
)

company_emissions_data_input_raw_cb <- company_emissions_data_input_raw_cb %>%
  dplyr::mutate(asset_type = "Bonds")

company_emissions_data_input_raw <- company_emissions_data_input_raw_eq %>%
  dplyr::bind_rows(company_emissions_data_input_raw_cb)

company_emissions_data_input_raw <- company_emissions_data_input_raw %>%
  dplyr::select(
    .data$investor_name, .data$portfolio_name, .data$company_name, .data$id,
    .data$scenario, .data$allocation, .data$asset_type, .data$scenario_geography,
    .data$equity_market, .data$year, .data$financial_sector, .data$ald_sector,
    .data$technology, .data$plan_tech_prod, .data$plan_emission_factor,
    .data$scen_tech_prod, .data$scen_emission_factor, .data$plan_carsten
    # TODO: unit emissions???
  ) %>%
  dplyr::filter(
    .data$scenario %in% .env$scenarios,
    .data$ald_sector %in% .env$sectors,
    .data$technology %in% .env$technologies,
    .data$year %in% seq(.env$start_year, .env$start_year + .env$horizon),
    .data$allocation == "portfolio_weight",
    .data$equity_market == "GlobalMarket",
    .data$scenario_geography == "Global" # TODO what about Global Aggregate?
  ) %>%
  dplyr::mutate(company_name = tolower(.data$company_name)) %>%
  dplyr::distinct_all()

# ADO 1540 - temporary fix: indistinguishable SDS scenarios when source is missing. make average across the entries..
company_emissions_data_input_raw <- company_emissions_data_input_raw %>%
  dplyr::group_by(
    .data$investor_name, .data$portfolio_name, .data$company_name, .data$id,
    .data$scenario, .data$allocation, .data$asset_type, .data$scenario_geography,
    .data$equity_market, .data$year, .data$financial_sector, .data$ald_sector,
    .data$technology
  ) %>%
  dplyr::summarise(
    plan_tech_prod = mean(.data$plan_tech_prod, na.rm = TRUE),
    plan_emission_factor = mean(.data$plan_emission_factor, na.rm = TRUE),
    scen_tech_prod = mean(.data$scen_tech_prod, na.rm = TRUE),
    scen_emission_factor = mean(.data$scen_emission_factor, na.rm = TRUE),
    plan_carsten = mean(.data$plan_carsten, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup()

company_emissions_data_input <- company_emissions_data_input_raw %>%
  dplyr::mutate(
    plan_emissions = .data$plan_tech_prod * .data$plan_emission_factor,
    scen_emissions = .data$scen_tech_prod * .data$scen_emission_factor
  ) %>%
  dplyr::group_by(
    .data$investor_name, .data$portfolio_name, .data$company_name, .data$id,
    .data$scenario, .data$allocation, .data$asset_type, .data$scenario_geography,
    .data$equity_market, .data$financial_sector, .data$ald_sector
    # TODO: by technology?
  ) %>%
  dplyr::summarise(
    plan_tech_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
    plan_emission_factor = mean(.data$plan_emission_factor, na.rm = TRUE),
    plan_emissions = sum(.data$plan_emissions, na.rm = TRUE),
    scen_tech_prod = sum(.data$scen_tech_prod, na.rm = TRUE),
    scen_emission_factor = mean(.data$scen_emission_factor, na.rm = TRUE),
    scen_emissions = sum(.data$scen_emissions, na.rm = TRUE),
    plan_carsten = sum(.data$plan_carsten, na.rm = TRUE), # TODO: change if we use tech level
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    current_production = .data$plan_tech_prod,
    allowed_production = .data$scen_tech_prod,
    actual_emissions = .data$plan_emissions,
    allowed_emission = .data$scen_emissions,
    avg_ef = .data$plan_emission_factor
  ) %>%
  dplyr::mutate(unit_emissions = "tCO2")

############################################################################
#### load and prepare company financial data--------------------------------
############################################################################

# ADO 1541 - read ebit data from financial data
company_ebit_data_input <- readr::read_csv(
  file.path(
    r2dii.utils::path_dropbox_2dii(
      "PortCheck", "00_Data", "02_FinancialData", "2019Q1", "EQS.Full.2019Q1.csv"
    )
  ),
  col_types = readr::cols_only(
    Name = "c",
    ISIN = "c",
    Curncy = "c",
    Revenue = "d",
    EBIT = "d"
  )
)

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::mutate(
    company_name = tolower(.data$Name),
    currency = toupper(.data$Curncy)
  ) %>%
  dplyr::rename(
    ebit = .data$EBIT,
    isin = .data$ISIN
  ) %>%
  dplyr::select(.data$company_name, .data$isin, .data$ebit, .data$currency)

# ADO 1541 - transform ebit to ebit usd
currencies <- readRDS(file.path(data_location, "currencies.rda")) %>%
  dplyr::select(.data$Currency_abbr, .data$ExchangeRate_2019Q4)

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::inner_join(currencies, by = c("currency" = "Currency_abbr"))

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::mutate(
    ebit = .data$ebit * .data$ExchangeRate_2019Q4,
    currency = "USD"
  ) %>%
  dplyr::select(-.data$ExchangeRate_2019Q4)

# ADO 1541 - merge in sector
security_financial_data <- readRDS(
  file.path(path_db_analysis_inputs, "security_financial_data.rda")
)

security_financial_data <- security_financial_data %>%
  dplyr::select(.data$isin, .data$security_mapped_sector)

company_ebit_data_input <- company_ebit_data_input %>%
  inner_join(security_financial_data, by = c("isin"))

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::rename(sector = .data$security_mapped_sector)

############################################################################
#### load and prepare carbon delta plus damages data------------------------
############################################################################
carbon_delta_plus_damages <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("delta_carbon_budget.csv")
  ),
  col_types = "ccdddd"
)

############################################################################
#### load and prepare historical emissions data-----------------------------
############################################################################
company_historical_emissions <- read_csv(
  file.path(
    get_st_data_path(),
    data_path("historical_emissions_heede_2017.csv")
  ),
  col_types = "cdddd"
) %>%
  dplyr::mutate(
    dplyr::across(c(scope_1, scope_3, scope_1_plus_3), ~ .x * 1000000),
    company_name = tolower(.data$company_name)
  )


############################################################################
#### combine and wrangle company data---------------------------------------
############################################################################
company_data <- company_emissions_data_input %>%
  # pivot_wider(
  #   names_from = scenario,
  #   values_from = c(allowed_emission, allowed_production)
  # ) %>%
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
    allowed_emission = allowed_production * avg_ef,
    allowed_emission_b2ds = dplyr::if_else(.data$scenario == "B2DS", allowed_production * avg_ef, 0),
    allowed_emission_sds = dplyr::if_else(.data$scenario == "SDS", allowed_production * avg_ef, 0),
    allowed_emission_nps = dplyr::if_else(.data$scenario == "NPS", allowed_production * avg_ef, 0),
    allowed_emission_cps = dplyr::if_else(.data$scenario == "CPS", allowed_production * avg_ef, 0)
  ) %>%
  mutate(
    overshoot_actual = actual_emissions - allowed_emission,
    overshoot_actual_b2ds = dplyr::if_else(.data$scenario == "B2DS", actual_emissions - allowed_emission_b2ds, 0),
    overshoot_actual_sds = dplyr::if_else(.data$scenario == "SDS", actual_emissions - allowed_emission_sds, 0),
    overshoot_actual_nps = dplyr::if_else(.data$scenario == "NPS", actual_emissions - allowed_emission_nps, 0),
    overshoot_actual_cps = dplyr::if_else(.data$scenario == "CPS", actual_emissions - allowed_emission_cps, 0)
  )

# ADO 1540 - removed the cap for overshoot deltas
company_carbon_budgets <- company_data_overshoot %>%
  mutate(
    delta_allowed_b2ds_sds = allowed_emission_sds - allowed_emission_b2ds,
    delta_allowed_sds_cps = allowed_emission_cps - allowed_emission_sds,
    delta_allowed_b2ds_cps = allowed_emission_cps - allowed_emission_b2ds,
    overshoot_delta_b2ds_sds = case_when(
      overshoot_actual_b2ds < 0 ~ 0,
      # overshoot_actual_b2ds > delta_allowed_b2ds_sds ~ delta_allowed_b2ds_sds,
      TRUE ~ overshoot_actual_b2ds
    ),
    overshoot_delta_b2ds_cps = case_when(
      overshoot_actual_b2ds < 0 ~ 0,
      # overshoot_actual_b2ds > delta_allowed_b2ds_cps ~ delta_allowed_b2ds_cps,
      TRUE ~ overshoot_actual_b2ds
    ),
    overshoot_delta_sds_cps = case_when(
      overshoot_actual_sds < 0 ~ 0,
      # overshoot_actual_sds > delta_allowed_sds_cps ~ delta_allowed_sds_cps,
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
    dplyr::mutate(
      scenario_name = cdd_scenario_i$litigation_scenario,
      damage_sds_b2ds = contribution_sds_b2ds * delta_carbon_damage_b2ds_sds,
      damage_cps_b2ds = contribution_cps_b2ds * delta_carbon_damage_b2ds_cps,
      damage_cps_sds = contribution_cps_sds * delta_carbon_damage_sds_cps,
      # TODO: make scenarios more easily selectable
      cdd_liability_total = damage_cps_b2ds * .env$scenario$exp_share_damages_paid,
      cdd_liability = cdd_liability_total /
        cdd_scenario_i$timeframe_emissions_overshoot,
      cdd_liability_perc_ebit = cdd_liability / ebit
    ) %>%
    dplyr::select(
      scenario_name,company_name, asset_type, scenario,
      sector, unit_emissions, actual_emissions,
      allowed_emission_b2ds, allowed_emission_sds, allowed_emission_cps,
      overshoot_delta_b2ds_sds, overshoot_delta_sds_cps, overshoot_delta_b2ds_cps,
      contribution_sds_b2ds, contribution_cps_sds, contribution_cps_b2ds,
      damage_sds_b2ds, damage_cps_sds, damage_cps_b2ds,
      cdd_liability, cdd_liability_total, ebit, cdd_liability_perc_ebit
    )

  cdd_results <- cdd_results %>%
    dplyr::bind_rows(cdd_company_i)

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

  # ADO 1540 - allow setting benchmark scenario via target variable
  scc_company_i <- company_carbon_budgets %>%
    dplyr::mutate(
      scenario_name = .env$scc_scenario_i$litigation_scenario,
      overshoot_actual = dplyr::case_when(
        .data$overshoot_actual < 0 ~ 0,
        TRUE ~ .data$overshoot_actual
      ),
      scc_liability_total =
        .data$overshoot_actual * scc_scenario_i$scc *
        .env$scc_scenario_i$exp_share_damages_paid,
      scc_liability = .data$scc_liability_total /
        .env$scc_scenario_i$timeframe_emissions_overshoot,
      scc_liability_perc_ebit = .data$scc_liability / .data$ebit
    ) %>%
    dplyr::select(
      .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
      .data$sector, .data$unit_emissions, .data$actual_emissions,
      .data$allowed_emission, .data$overshoot_actual, .data$scc_liability,
      .data$scc_liability_total, .data$ebit, .data$scc_liability_perc_ebit
    )

  scc_results <- scc_results %>%
    dplyr::bind_rows(scc_company_i)

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

  if(her_scenario_i$scc > 0 & her_scenario_i$past_yearly_costs_usd == 0) {

    her_company_i <- company_historical_emissions %>%
      left_join(company_ebit_data_input, by = c("company_name")) %>%
      filter(ebit > 0)

    her_company_i <- her_company_i %>%
      dplyr::mutate(
        scenario_name = her_scenario_i$litigation_scenario,
        her_liability = .data$scope_1_plus_3 *
          her_scenario_i$scc *
          her_scenario_i$exp_share_damages_paid,
        her_liability_total = her_liability,
        her_liability_perc_ebit = her_liability / ebit
      ) %>%
      dplyr::select(
        scenario_name, company_name, sector, #unit_emissions,
        actual_emissions = scope_1_plus_3, share_global_industrial_ghg,
        her_liability, her_liability_total, ebit, her_liability_perc_ebit
      )

    her_results <- her_results %>%
      dplyr::bind_rows(her_company_i)

  } else if(her_scenario_i$scc == 0 & her_scenario_i$past_yearly_costs_usd > 0) {

    her_company_i <- company_historical_emissions %>%
      dplyr::left_join(company_ebit_data_input, by = c("company_name")) %>%
      dplyr::filter(ebit > 0)

    her_company_i <- her_company_i %>%
      dplyr::mutate(
        scenario_name = her_scenario_i$litigation_scenario,
        her_liability = .data$share_global_industrial_ghg *
          her_scenario_i$past_yearly_costs_usd *
          backwards_years *
          her_scenario_i$exp_share_damages_paid,
        her_liability_total = her_liability,
        # TODO: percentage loss in HER model with total liability, others annual. Why?
        her_liability_perc_ebit = her_liability / ebit
      ) %>%
      dplyr::select(
        scenario_name, company_name, sector, #unit_emissions,
        actual_emissions = scope_1_plus_3, share_global_industrial_ghg,
        her_liability, her_liability_total, ebit, her_liability_perc_ebit
      )

    her_results <- her_results %>%
      dplyr::bind_rows(her_company_i)

  } else {
    next("Cannot process the scenario parameters for the HER scenario")
  }

}

her_results <- her_results %>%
  dplyr::mutate(
    asset_type = NA_character_,
    scenario = NA_character_
  )

her_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_her.csv")
)


#######



company_results <- cdd_results %>%
  select(
    scenario_name, company_name, asset_type, scenario,
    sector,
    liability = cdd_liability,
    liability_total = cdd_liability_total,
    ebit,
    liability_perc_ebit = cdd_liability_perc_ebit
  ) %>%
  bind_rows(
    scc_results %>%
      select(
        scenario_name, company_name, asset_type, scenario,
        sector,
        liability = scc_liability,
        liability_total = scc_liability_total,
        ebit,
        liability_perc_ebit = scc_liability_perc_ebit
      )
  ) %>% bind_rows(
    her_results %>%
      select(
        scenario_name, company_name, asset_type, scenario,
        sector,
        liability = her_liability,
        liability_total = her_liability_total,
        ebit,
        liability_perc_ebit = her_liability_perc_ebit # TODO: This is off!
      )
  )

# TODO: for company level impact, the sectors/types need to by summed by comp
company_results %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company.csv")
)


#-Impact on share price----------------------------

start_year <- cfg$AnalysisPeriod$Years.Startyear
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
    cols = -c(scenario_name, company_name, asset_type, scenario, sector, liability,
              liability_total, ebit, liability_perc_ebit, years_to_litigation,
              settlement_factor, settlement),
    names_to = "year",
    values_to = "dividends"
  ) %>%
  arrange(scenario_name, company_name, asset_type, scenario, sector, year) %>%
  group_by(scenario_name, company_name, asset_type, scenario, sector) %>%
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

if(reset_post_settlement == "start") {
  company_results_dcf <- company_results_dcf %>%
    group_by(scenario_name, company_name, asset_type, scenario, sector) %>%
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
          (1 + growth_rate) ^
          dplyr::lag(.data$t_calc, n = years_to_litigation_event + 1),
        .data$dividends_litigation
      )
    ) %>%
    ungroup()
}

company_results_dcf <- company_results_dcf %>%
  group_by(scenario_name, company_name, asset_type, scenario, sector) %>%
  mutate(
    discounted_dividends = dividends / ((1 + discount_rate)^t_calc),
    discounted_dividends_litigation = dividends_litigation / ((1 + discount_rate)^t_calc)
  ) %>%
  ungroup()



# calculate NPV

company_results_npv <- company_results_dcf %>%
  group_by(scenario_name, company_name, asset_type, scenario, sector) %>%
  summarise(
    npv_baseline = sum(discounted_dividends, na.rm = TRUE) * (1 + terminal_value),
    npv_litigation = sum(discounted_dividends_litigation, na.rm = TRUE) * (1 + terminal_value),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    npv_litigation = dplyr::if_else(.data$npv_litigation < 0, 0, .data$npv_litigation),
    percentage_value_change = round((npv_litigation - npv_baseline) / npv_baseline, 4)
  )

company_results_npv %>% write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_value_change.csv")
)


#-Portfolio Level Calculation----------------------


technology_share_comp <- company_emissions_data_input %>%
  dplyr::select(
    .data$investor_name, .data$portfolio_name, .data$company_name, .data$id,
    .data$asset_type, .data$scenario,
    .data$scenario_geography, .data$ald_sector, #.data$technology,
    .data$plan_carsten
  )

sector_exposures <- readRDS(file.path(proc_input_path, glue::glue("{project_name}_overview_portfolio.rda")))

# portfolio aum for equity, value in USD
# TODO: check if this might not be in EUR for EIOPA case
port_aum_equity <- sector_exposures %>%
  dplyr::filter(.data$asset_type == "Equity") %>%
  pull(.data$asset_value_usd) %>%
  unique()

# portfolio aum for bonds, value in USD
# TODO: check if this might not be in EUR for EIOPA case
port_aum_bonds <- sector_exposures %>%
  dplyr::filter(.data$asset_type == "Bonds") %>%
  pull(.data$asset_value_usd) %>%
  unique()

technology_exposure <- technology_share_comp %>%
  mutate(
    portfolio_aum = dplyr::case_when(
      .data$asset_type == "Equity" ~ port_aum_equity,
      .data$asset_type == "Bonds" ~ port_aum_bonds,
      TRUE ~ 0
    ),
    company_tech_exposure = portfolio_aum * plan_carsten
  )


# TODO: merge portfolio comp-tech exposures with company results (percentage loss)
# TODO: same for SCC 20 and 40??
portfolio_liabilities <- company_results_npv %>%
  dplyr::inner_join(
    technology_exposure,
    by = c("company_name", "asset_type", "scenario", "sector" = "ald_sector")
  )

portfolio_liabilities <- portfolio_liabilities %>%
  dplyr::mutate(
    value_change = dplyr::if_else(
      .data$asset_type == "Bonds",
      .data$company_tech_exposure * .data$percentage_value_change * .env$flat_multiplier,
      .data$company_tech_exposure * .data$percentage_value_change
    )
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
