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
source(file.path("R","stress_test_model_functions.R"))

################
# CUSTOM FUNCTIONS
################
set_project_paths <- function(project_name, twodii_internal, project_location_ext) {
  portcheck_v2_path <<- path_dropbox_2dii("PortCheck_v2")
  project_location <<- ifelse(twodii_internal,
                              path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name),
                              paste0(project_location_ext, "/", project_name)
  )

  log_path <<- paste0(project_location, "/00_Log_Files")
  par_file_path <<- paste0(project_location, "/10_Parameter_File")
  raw_input_path <<- paste0(project_location, "/20_Raw_Inputs")
  proc_input_path <<- paste0(project_location, "/30_Processed_Inputs")
  results_path <<- paste0(project_location, "/40_Results")
  outputs_path <<- paste0(project_location, "/50_Outputs")
}

# checks validity of project config
# FIXME:
# check_valid_cfg <- function(cfg) stopifnot(exists("cfg") == TRUE)
# testthat::expect_error(check_valid_cfg())
# #> Error: `check_valid_cfg()` did not throw an error.
check_valid_cfg <- function(cfg, expected_no_args = 3) {
  stopifnot(exists("cfg") == TRUE)
  stopifnot(cfg %>% class() == "list")
  stopifnot(cfg %>% length() == expected_no_args)

  stopifnot(cfg$project_name %>% is.character() == TRUE)
  stopifnot(cfg$project_internal$twodii_internal %>% is.logical() == TRUE)

  invisible(cfg)
}

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


cfg_st <- config::get(file = "nonstandard/st_project_settings.yml")
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
target_currency <- cfg_litigation_params$litigation$target_currency

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
#### load and prepare currency data-----------------------------------------
############################################################################
currency_data <- fst::read_fst(
  file.path(
    Sys.getenv("PACTA_DATA_PATH"), "2019Q4", "cleaned_files", "currencies.fst"
  )
)
currency_data <- currency_data %>%
  dplyr::filter(.data$currency == .env$target_currency)


############################################################################
#### load and prepare litigation risk scenarios-----------------------------
############################################################################
litigation_risk_scenarios <- readr::read_csv(
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
company_emissions_data_input_raw_eq <- readr::read_rds(
  file.path(
    results_path, investor_name_equity, "Equity_results_company.rda"
  )
)

company_emissions_data_input_raw_eq <- company_emissions_data_input_raw_eq %>%
  dplyr::mutate(asset_type = "Equity")

company_emissions_data_input_raw_cb <- readr::read_rds(
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
  dplyr::mutate(
    ald_sector = dplyr::if_else(
      .data$ald_sector %in% c("Cement", "Steel"),
      "Cement&Steel",
      .data$ald_sector
    )
  ) %>%
  dplyr::distinct_all()

# ADO 1540 - remove zero and NA entries
company_emissions_data_input_raw <- company_emissions_data_input_raw %>%
  dplyr::filter(
    .data$plan_tech_prod > 0,
    !is.na(.data$plan_emission_factor)
  )

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
    plan_emission_factor = weighted.mean(.data$plan_emission_factor, w = .data$plan_tech_prod, na.rm = TRUE),
    scen_emission_factor = weighted.mean(.data$scen_emission_factor, w = .data$scen_tech_prod, na.rm = TRUE),
    plan_tech_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
    plan_emissions = sum(.data$plan_emissions, na.rm = TRUE),
    scen_tech_prod = sum(.data$scen_tech_prod, na.rm = TRUE),
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
currencies <- readr::read_rds(file.path(data_location, "currencies.rda")) %>%
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
security_financial_data <- readr::read_rds(
  file.path(path_db_analysis_inputs, "security_financial_data.rda")
)

security_financial_data <- security_financial_data %>%
  dplyr::select(.data$isin, .data$security_mapped_sector)

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::inner_join(security_financial_data, by = c("isin"))

company_ebit_data_input <- company_ebit_data_input %>%
  dplyr::rename(sector = .data$security_mapped_sector)

############################################################################
#### load and prepare carbon delta plus damages data------------------------
############################################################################
carbon_delta_plus_damages <- readr::read_csv(
  file.path(
    get_st_data_path(),
    data_path("delta_carbon_budget.csv")
  ),
  col_types = "ccdddd"
)

############################################################################
#### load and prepare historical emissions data-----------------------------
############################################################################
company_historical_emissions <- readr::read_csv(
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
  dplyr::left_join(
    company_ebit_data_input,
    by = "company_name"
  ) %>%
  dplyr::filter(.data$ebit > 0)
# TODO: There are still duplicates because some companies are in multiple sectors


#-Company Level Calculation-----------------------------

company_data_overshoot <- company_data %>%
  dplyr::mutate(
    actual_emissions = .data$current_production * .data$avg_ef,
    allowed_emission = .data$allowed_production * .data$scen_emission_factor,
    allowed_emission_b2ds = dplyr::if_else(
      .data$scenario == "B2DS", .data$allowed_production * .data$scen_emission_factor, 0
    ),
    allowed_emission_sds = dplyr::if_else(
      .data$scenario == "SDS", .data$allowed_production * .data$scen_emission_factor, 0
    ),
    allowed_emission_nps = dplyr::if_else(
      .data$scenario == "NPS", .data$allowed_production * .data$scen_emission_factor, 0
    ),
    allowed_emission_cps = dplyr::if_else(
      .data$scenario == "CPS", .data$allowed_production * .data$scen_emission_factor, 0
    )
  ) %>%
  dplyr::mutate(
    overshoot_actual = .data$actual_emissions - .data$allowed_emission,
    overshoot_actual_b2ds = dplyr::if_else(
      .data$scenario == "B2DS", .data$actual_emissions - .data$allowed_emission_b2ds, 0
    ),
    overshoot_actual_sds = dplyr::if_else(
      .data$scenario == "SDS", .data$actual_emissions - .data$allowed_emission_sds, 0
    ),
    overshoot_actual_nps = dplyr::if_else(
      .data$scenario == "NPS", .data$actual_emissions - .data$allowed_emission_nps, 0
    ),
    overshoot_actual_cps = dplyr::if_else(
      .data$scenario == "CPS", .data$actual_emissions - .data$allowed_emission_cps, 0
    )
  )

# ADO 1540 - removed the cap for overshoot deltas
company_carbon_budgets <- company_data_overshoot %>%
  dplyr::mutate(
    delta_allowed_b2ds_sds = .data$allowed_emission_sds - .data$allowed_emission_b2ds,
    delta_allowed_sds_cps = .data$allowed_emission_cps - .data$allowed_emission_sds,
    delta_allowed_b2ds_cps = .data$allowed_emission_cps - .data$allowed_emission_b2ds,
    overshoot_delta_b2ds_sds = dplyr::case_when(
      .data$overshoot_actual_b2ds < 0 ~ 0,
      # overshoot_actual_b2ds > delta_allowed_b2ds_sds ~ delta_allowed_b2ds_sds,
      TRUE ~ .data$overshoot_actual_b2ds
    ),
    overshoot_delta_b2ds_cps = dplyr::case_when(
      .data$overshoot_actual_b2ds < 0 ~ 0,
      # overshoot_actual_b2ds > delta_allowed_b2ds_cps ~ delta_allowed_b2ds_cps,
      TRUE ~ .data$overshoot_actual_b2ds
    ),
    overshoot_delta_sds_cps = dplyr::case_when(
      .data$overshoot_actual_sds < 0 ~ 0,
      # overshoot_actual_sds > delta_allowed_sds_cps ~ delta_allowed_sds_cps,
      TRUE ~ .data$overshoot_actual_sds
    )
  )


# Carbon Delta Damages (CDD) liability model

scenario <- litigation_risk_scenarios %>%
  dplyr::filter(.data$model == "CDD")

chance_within_target <- cfg_litigation_params$litigation$chance_within_target

cdd_carbon_budget_plus_damages <- carbon_delta_plus_damages %>%
  dplyr::filter(.data$likelihood_stay_within_target == .env$chance_within_target)

delta_carbon_budget_b2ds_sds <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario == "SDS") %>%
  dplyr::pull(.data$delta_carbon_budget)

delta_carbon_budget_b2ds_cps <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario %in% c("SDS", "CPS")) %>%
  dplyr::summarise(
    delta_carbon_budget = sum(.data$delta_carbon_budget, na.rm = TRUE)
  ) %>%
  dplyr::pull()

delta_carbon_budget_sds_cps <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario == "CPS") %>%
  dplyr::pull(.data$delta_carbon_budget)

cdd_company_carbon_budgets <- company_carbon_budgets %>%
  dplyr::mutate(
    contribution_sds_b2ds = .data$overshoot_delta_b2ds_sds /
      .env$delta_carbon_budget_b2ds_sds,
    contribution_cps_b2ds = .data$overshoot_delta_b2ds_cps /
      .env$delta_carbon_budget_b2ds_cps,
    contribution_cps_sds = .data$overshoot_delta_sds_cps /
      .env$delta_carbon_budget_sds_cps
  )

delta_carbon_damage_b2ds_sds <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario == "SDS") %>%
  dplyr::pull(.data$delta_econ_damages_usd)

delta_carbon_damage_b2ds_cps <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario %in% c("SDS", "CPS")) %>%
  dplyr::summarise(
    delta_econ_damages_usd = sum(.data$delta_econ_damages_usd, na.rm = TRUE)
  ) %>%
  dplyr::pull()

delta_carbon_damage_sds_cps <- cdd_carbon_budget_plus_damages %>%
  dplyr::filter(.data$target_scenario == "CPS") %>%
  dplyr::pull(.data$delta_econ_damages_usd)

# TODO: initialise using a list or df with correct number of entries
cdd_results <- c()

for (i in seq(1, nrow(scenario))) {

  cdd_scenario_i <- scenario[i, ]

  cdd_company_i <- cdd_company_carbon_budgets %>%
    dplyr::mutate(
      scenario_name = .env$cdd_scenario_i$litigation_scenario,
      damage_sds_b2ds = .data$contribution_sds_b2ds * .env$delta_carbon_damage_b2ds_sds,
      damage_cps_b2ds = .data$contribution_cps_b2ds * .env$delta_carbon_damage_b2ds_cps,
      damage_cps_sds = .data$contribution_cps_sds * .env$delta_carbon_damage_sds_cps,
      # TODO: make scenarios more easily selectable
      cdd_liability_total = .data$damage_cps_b2ds * .env$scenario$exp_share_damages_paid,
      cdd_liability = .data$cdd_liability_total /
        .env$cdd_scenario_i$timeframe_emissions_overshoot,
      cdd_liability_perc_ebit = .data$cdd_liability / .data$ebit
    ) %>%
    dplyr::select(
      .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
      sector = .data$sector, .data$unit_emissions, .data$actual_emissions,
      .data$allowed_emission_b2ds, .data$allowed_emission_sds,
      .data$allowed_emission_cps,
      .data$overshoot_delta_b2ds_sds, .data$overshoot_delta_sds_cps,
      .data$overshoot_delta_b2ds_cps,
      .data$contribution_sds_b2ds, .data$contribution_cps_sds,
      .data$contribution_cps_b2ds,
      .data$damage_sds_b2ds, .data$damage_cps_sds, .data$damage_cps_b2ds,
      .data$cdd_liability, .data$cdd_liability_total, .data$ebit,
      .data$cdd_liability_perc_ebit
    )

  cdd_results <- cdd_results %>%
    dplyr::bind_rows(cdd_company_i)

}

# ADO 1544 - transform currency back to target currency
cdd_results <- cdd_results %>%
  dplyr::mutate(
    across(
      c(.data$cdd_liability, .data$cdd_liability_total, .data$ebit),
      ~ .x / .env$currency_data$exchange_rate
    )
  )

cdd_results %>% readr::write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_cdd.csv")
)



# Social Cost of Carbon (SCC) liability model

scenario <- litigation_risk_scenarios %>%
  dplyr::filter(.data$model == "SCC")

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
      sector = .data$ald_sector, .data$unit_emissions, .data$actual_emissions,
      .data$allowed_emission, .data$overshoot_actual, .data$scc_liability,
      .data$scc_liability_total, .data$ebit, .data$scc_liability_perc_ebit
    )

  scc_results <- scc_results %>%
    dplyr::bind_rows(scc_company_i)

}

# ADO 1544 - transform currency back to target currency
scc_results <- scc_results %>%
  dplyr::mutate(
    across(
      c(.data$scc_liability, .data$scc_liability_total, .data$ebit),
      ~ .x / .env$currency_data$exchange_rate
    )
  )

scc_results %>% readr::write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_scc.csv")
)


# Historical Emissions Responsibility (HER) model

scenario <- litigation_risk_scenarios %>%
  dplyr::filter(.data$model == "HER")

backwards_years <- cfg_litigation_params$litigation$backwards_years

# TODO: initialise using a list or df with correct number of entries
her_results <- c()

for (i in seq(1, nrow(scenario))) {

  her_scenario_i <- scenario[i, ]

  if(her_scenario_i$scc > 0 & her_scenario_i$past_yearly_costs_usd == 0) {

    her_company_i <- company_historical_emissions %>%
      dplyr::left_join(company_ebit_data_input, by = c("company_name")) %>%
      dplyr::filter(.data$ebit > 0)

    her_company_i <- her_company_i %>%
      dplyr::mutate(
        scenario_name = .env$her_scenario_i$litigation_scenario,
        her_liability = .data$scope_1_plus_3 *
          .env$her_scenario_i$scc *
          .env$her_scenario_i$exp_share_damages_paid,
        her_liability_total = .data$her_liability,
        her_liability_perc_ebit = .data$her_liability / .data$ebit
      ) %>%
      dplyr::select(
        .data$scenario_name, .data$company_name, .data$sector, #unit_emissions,
        actual_emissions = .data$scope_1_plus_3, .data$share_global_industrial_ghg,
        .data$her_liability, .data$her_liability_total, .data$ebit,
        .data$her_liability_perc_ebit
      )

    her_results <- her_results %>%
      dplyr::bind_rows(her_company_i)

  } else if(her_scenario_i$scc == 0 & her_scenario_i$past_yearly_costs_usd > 0) {

    her_company_i <- company_historical_emissions %>%
      dplyr::left_join(company_ebit_data_input, by = c("company_name")) %>%
      dplyr::filter(.data$ebit > 0)

    her_company_i <- her_company_i %>%
      dplyr::mutate(
        scenario_name = .env$her_scenario_i$litigation_scenario,
        her_liability = .data$share_global_industrial_ghg *
          .env$her_scenario_i$past_yearly_costs_usd *
          .env$backwards_years *
          .env$her_scenario_i$exp_share_damages_paid,
        her_liability_total = .data$her_liability,
        # TODO: percentage loss in HER model with total liability, others annual. Why?
        her_liability_perc_ebit = .data$her_liability / .data$ebit
      ) %>%
      dplyr::select(
        .data$scenario_name, .data$company_name, .data$sector, #unit_emissions,
        actual_emissions = .data$scope_1_plus_3, .data$share_global_industrial_ghg,
        .data$her_liability, .data$her_liability_total, .data$ebit,
        .data$her_liability_perc_ebit
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

# ADO 1544 - transform currency back to target currency
her_results <- her_results %>%
  dplyr::mutate(
    across(
      c(.data$her_liability, .data$her_liability_total, .data$ebit),
      ~ .x / .env$currency_data$exchange_rate
    )
  )

her_results %>% readr::write_csv(
  file.path(project_location, "40_Results", "litigation_risk_company_her.csv")
)


#######



company_results <- cdd_results %>%
  dplyr::select(
    .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
    .data$sector,
    liability = .data$cdd_liability,
    liability_total = .data$cdd_liability_total,
    .data$ebit,
    liability_perc_ebit = .data$cdd_liability_perc_ebit
  ) %>%
  dplyr::bind_rows(
    scc_results %>%
      dplyr::select(
        .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
        .data$sector,
        liability = .data$scc_liability,
        liability_total = .data$scc_liability_total,
        .data$ebit,
        liability_perc_ebit = .data$scc_liability_perc_ebit
      )
  ) %>%
  dplyr::bind_rows(
    her_results %>%
      dplyr::select(
        .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
        .data$sector,
        liability = .data$her_liability,
        liability_total = .data$her_liability_total,
        .data$ebit,
        liability_perc_ebit = .data$her_liability_perc_ebit # TODO: This is off!
      )
  )

# TODO: for company level impact, the sectors/types need to by summed by comp
company_results %>% readr::write_csv(
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
  dplyr::mutate(
    years_to_litigation = .env$years_to_litigation_event,
    settlement_factor = .env$settlement_factor,
    # TODO: how o calculate this for HER model? liability should not increase with years to litigation?
    settlement = .data$liability * .data$years_to_litigation *
      .data$settlement_factor
  )

df_timeframe <- tibble::tibble(
    timeframe = seq(start_year, end_year),
    value = NA_real_
  ) %>%
  tidyr::pivot_wider(
    names_from = timeframe,
    values_from = value
  )


# TODO: use calculated target profits from transition risk module after settlement
company_results_dcf <- company_results %>%
  dplyr::bind_cols(
    df_timeframe
  ) %>%
  tidyr::pivot_longer(
    cols = -c(
      scenario_name, company_name, asset_type, scenario, sector, liability,
      liability_total, ebit, liability_perc_ebit, years_to_litigation,
      settlement_factor, settlement
    ),
    names_to = "year",
    values_to = "dividends"
  )

company_results_dcf <- company_results_dcf %>%
  dplyr::arrange(
    .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
    .data$sector, .data$year
  ) %>%
  dplyr::group_by(
    .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
    .data$sector
  ) %>%
  dplyr::mutate(
    t_calc = seq(0, (dplyr::n() - 1)),
    dividends = .data$ebit * (1 + .env$growth_rate) ^ .data$t_calc,
    dividends_litigation = dplyr::if_else(
      # year == start_year + years_to_litigation_event,
      year == .env$start_year + .data$years_to_litigation,
      .data$dividends - .data$settlement,
      .data$dividends
    )
  ) %>%
  dplyr::ungroup()

reset_post_settlement <- cfg_litigation_params$litigation$reset_post_settlement

if(reset_post_settlement == "start") {
  company_results_dcf <- company_results_dcf %>%
    dplyr::group_by(
      .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
      .data$sector
    ) %>%
    dplyr::mutate(
      dividends_litigation = dplyr::if_else(
        # .data$year == start_year + years_to_litigation_event + 1 &
        #   .data$liability > 0,
        .data$year == .env$start_year + .env$years_to_litigation_event + 1 &
          .data$liability > 0,
        .data$ebit,
        .data$dividends_litigation
      )
    ) %>%
    dplyr::mutate(
      dividends_litigation = dplyr::if_else(
        # .data$year > start_year + years_to_litigation_event + 1 &
        #   .data$liability > 0,
        .data$year > .env$start_year + .env$years_to_litigation_event + 1 &
          .data$liability > 0,
        .data$ebit * (1 + .env$growth_rate) ^
          dplyr::lag(.data$t_calc, n = .env$years_to_litigation_event + 1),
        .data$dividends_litigation
      )
    ) %>%
    dplyr::ungroup()
}

company_results_dcf <- company_results_dcf %>%
  dplyr::group_by(
    .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
    .data$sector
  ) %>%
  dplyr::mutate(
    discounted_dividends = .data$dividends /
      ((1 + .env$discount_rate) ^ .data$t_calc),
    discounted_dividends_litigation = .data$dividends_litigation /
      ((1 + .env$discount_rate) ^ .data$t_calc)
  ) %>%
  dplyr::ungroup()



# calculate NPV

company_results_npv <- company_results_dcf %>%
  dplyr::group_by(
    .data$scenario_name, .data$company_name, .data$asset_type, .data$scenario,
    .data$sector
  ) %>%
  dplyr::summarise(
    npv_baseline = sum(.data$discounted_dividends, na.rm = TRUE) *
      (1 + .env$terminal_value),
    npv_litigation = sum(.data$discounted_dividends_litigation, na.rm = TRUE) *
      (1 + .env$terminal_value),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    npv_litigation = dplyr::if_else(.data$npv_litigation < 0, 0, .data$npv_litigation),
    percentage_value_change = round(
      (.data$npv_litigation - .data$npv_baseline) / .data$npv_baseline, 6
    )
  )

company_results_npv %>% readr::write_csv(
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

sector_exposures <- readr::read_rds(file.path(proc_input_path, glue::glue("{project_name}_overview_portfolio.rda")))

# ADO 1544 - transform currency back to target currency
sector_exposures <- sector_exposures %>%
  dplyr::mutate(
    dplyr::across(
      c(.data$valid_value_usd, .data$asset_value_usd, .data$portfolio_value_usd),
      ~ .x / .env$currency_data$exchange_rate
    )
  )

# portfolio aum for equity, value in target currency (ADO 1544)
port_aum_equity <- sector_exposures %>%
  dplyr::filter(.data$asset_type == "Equity") %>%
  dplyr::pull(.data$asset_value_usd) %>%
  unique()

# portfolio aum for bonds, value in target currency (ADO 1544)
port_aum_bonds <- sector_exposures %>%
  dplyr::filter(.data$asset_type == "Bonds") %>%
  dplyr::pull(.data$asset_value_usd) %>%
  unique()

technology_exposure <- technology_share_comp %>%
  dplyr::mutate(
    portfolio_aum = dplyr::case_when(
      .data$asset_type == "Equity" ~ .env$port_aum_equity,
      .data$asset_type == "Bonds" ~ .env$port_aum_bonds,
      TRUE ~ 0
    ),
    company_exposure = .data$portfolio_aum * .data$plan_carsten
    # company_tech_exposure = .data$portfolio_aum * .data$plan_carsten
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
    company_value_change = dplyr::if_else(
      .data$asset_type == "Bonds",
      .data$company_exposure * .data$percentage_value_change * .env$flat_multiplier,
      .data$company_exposure * .data$percentage_value_change
      # .data$company_tech_exposure * .data$percentage_value_change * .env$flat_multiplier,
      # .data$company_tech_exposure * .data$percentage_value_change
    )
  ) %>%
  dplyr::group_by(
    .data$investor_name, .data$portfolio_name, .data$scenario_name,
    .data$scenario_geography, .data$asset_type, .data$scenario, .data$sector
  ) %>%
  dplyr::mutate(
    sector_exposure = sum(.data$company_exposure, na.rm = TRUE),
    sector_value_change = sum(.data$company_value_change, na.rm = TRUE),
    sector_percentage_value_change = .data$sector_value_change / .data$sector_exposure
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    .data$investor_name, .data$portfolio_name, .data$scenario_name,
    .data$scenario_geography, .data$asset_type, .data$scenario
  ) %>%
  dplyr::mutate(
    portfolio_value_change = sum(company_value_change, na.rm = TRUE),
    portfolio_percentage_value_change = .data$portfolio_value_change / .data$portfolio_aum
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    .data$investor_name, .data$portfolio_name, .data$scenario_name, .data$asset_type,
    .data$company_name, .data$scenario_geography, .data$scenario, .data$sector,
    .data$company_exposure, .data$percentage_value_change, .data$company_value_change,
    .data$sector_exposure, .data$sector_percentage_value_change, .data$sector_value_change,
    .data$portfolio_aum, .data$portfolio_percentage_value_change, .data$portfolio_value_change
  )


portfolio_liabilities %>% readr::write_csv(
  file.path(project_location, "40_Results", "litigation_risk_portfolio_impact.csv")
)



### visualization

viz_company_results <- company_results %>%
  dplyr::filter(.data$liability_perc_ebit >= 0) %>%
  dplyr::group_by(.data$company_name) %>%
  dplyr::mutate(max_liability_perc = max(.data$liability_perc_ebit, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    company_name = forcats::fct_reorder(
      .data$company_name, dplyr::desc(.data$max_liability_perc)
    )
  ) %>%
  dplyr::filter(.data$company_name %in% unique(.env$technology_exposure$company_name)) %>%
  dplyr::filter(.data$scenario == "SDS" | .data$scenario_name == "HER_CDD_TMS")

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
subset_companies <- unique(technology_exposure$company_name)

subset_models <- c(
  # "CDD_TMS",
  # "HER_CDD_TMS",
  "SCC_20_TMS",
  "SCC_40_TMS",
  "SCC_100_TMS"
)

viz_company_results_subset <- viz_company_results %>%
  dplyr::filter(
    # .data$company_name %in% .env$subset_companies &
      .data$scenario_name %in% .env$subset_models
  ) %>%
  dplyr::group_by(.data$company_name) %>%
  dplyr::mutate(max_liability_perc = max(.data$liability_perc_ebit, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    company_name = forcats::fct_reorder(
      .data$company_name, dplyr::desc(.data$max_liability_perc)
    )
  )

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
  dplyr::group_by(.data$company_name) %>%
  dplyr::mutate(max_liability = max(.data$liability_total, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    company_name = forcats::fct_reorder(
      .data$company_name, dplyr::desc(.data$max_liability)
    )
  )


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

#
min_value_change <- abs(min(portfolio_liabilities$company_value_change, na.rm = TRUE)) * 10

portfolio_liabilities_plot <- portfolio_liabilities %>%
  dplyr::filter(
    .data$asset_type == "Bonds",
    .data$scenario_name %in% .env$subset_models,
    .data$scenario == "SDS"
  ) %>%
  ggplot2::ggplot(aes(x = company_name)) +
  geom_point(aes(y = .data$percentage_value_change), alpha = 0.5) +
  geom_col(aes(y = .data$company_value_change / .env$min_value_change, fill = .data$sector), alpha = 0.5) +
  scale_y_continuous(
    name = "Percentage value change, as ratio (points)",
    limits = c(-0.1, 0),
    sec.axis = sec_axis(~ . * min_value_change, name = glue::glue("Company value change, in {target_currency} (bars)"))
  ) +
  facet_grid(cols = vars(scenario_name)) +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  ggtitle("Impact of liability risk on portfolio")

min_value_change_sector <- abs(min(portfolio_liabilities$sector_value_change, na.rm = TRUE)) * 10

portfolio_liabilities_sector_plot <- portfolio_liabilities %>%
  dplyr::group_by(investor_name, portfolio_name, scenario_name, asset_type, scenario_geography, scenario, sector) %>%
  dplyr::summarise(
    sector_percentage_value_change = max(sector_percentage_value_change, na.rm = TRUE),
    sector_value_change = max(sector_value_change, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    .data$asset_type == "Bonds",
    .data$scenario_name %in% .env$subset_models,
    .data$scenario == "SDS"
  ) %>%
  ggplot2::ggplot(aes(x = sector)) +
  geom_point(aes(y = .data$sector_percentage_value_change), alpha = 0.5) +
  geom_col(aes(y = .data$sector_value_change / .env$min_value_change_sector, fill = .data$sector), alpha = 0.5) +
  scale_y_continuous(
    name = "Percentage value change, as ratio (points)",
    limits = c(-0.1, 0),
    sec.axis = sec_axis(~ . * min_value_change_sector, name = glue::glue("Company value change, in {target_currency} (bars)"))
  ) +
  facet_grid(cols = vars(scenario_name)) +
  theme(
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  ggtitle("Impact of liability risk on portfolio")
