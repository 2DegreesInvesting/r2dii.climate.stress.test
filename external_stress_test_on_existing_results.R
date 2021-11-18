## Project Initialisation

library(dplyr)
library(forcats)
library(ggplot2)
library(purrr)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)
library(zoo)

source(file.path("R","utils.R"))
source(file.path("R","set_paths.R"))
source(file.path("R","get_st_data_path.R"))
source(file.path("R","stress_test_model_functions.R"))
source("0_global_functions_st.R")
source("0_portfolio_input_check_functions.R")

################
# INPUT VARIABLES
################

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
check_valid_cfg(cfg_st, expected_no_args = 5)
project_name <- cfg_st$project_name
twodii_internal <- cfg_st$project_internal$twodii_internal
project_location_ext <- cfg_st$project_internal$project_location_ext
price_data_version <- cfg_st$price_data_version

data_location <- file.path(get_st_data_path(), data_path())

# set input path
set_project_paths(
  project_name = project_name,
  twodii_internal = twodii_internal,
  project_location_ext = project_location_ext
)

# THIS NEEDS TO BE INVESTIGATED! PROBABLY LOOP OVER INV OR ALLOW SPECIFICATION IN CONFIG
# investorname_bonds <- 'Meta Investor'
# investorname_equity <- 'Meta Investor'
investor_name_filter <- "Meta Investor"

#### Analysis Parameters----------------------------------------
# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis

set_global_parameters(file_path = file.path(par_file_path, "AnalysisParameters.yml"))
analysis_inputs_path <- set_analysis_inputs_path(
  twodii_internal = twodii_internal,
  data_location_ext = data_location_ext,
  dataprep_ref = dataprep_timestamp
)

# cfg <- config::get(file = paste0(project_location,'/10_Parameter_File/AnalysisParameters.yml'))
# OPEN: check_valid_cfg() not applicable here
# start_year          <- cfg$AnalysisPeriod$Years.Startyear
# dataprep_timestamp  <- cfg$TimeStamps$DataPrep.Timestamp # is this being used for anything???

##### Filters----------------------------------------
# The filter settings should comply with the filters from the parent PACTA project as per default
# There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
# move to config once mechanism to include/exclude filters from original pacta project exists

#### OPEN: This could largely be taken from cfg file. No apparent reason why not.
scenario_geography_filter <- "Global"
# scenario_geography_filter <- cfg$Lists$Scenario.Geography.List
scenarios <- c("NPSRTS", "SDS", "CPS", "B2DS")
# scenarios <- cfg$Large.Universe.Filter$SCENARIO.FILTER

allocation_method_equity <- "portfolio_weight"
equity_market_filter <- equity_market_list

sectors <- c("Power", "Oil&Gas", "Coal", "Automotive")
# setors <- cfg$Large.Universe.Filter$SECTOR.FILTER
technologies <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)
# technologies <- cfg$Lists$Technology.List


## Project start
# File List Check for External Data Requirements:
# security_financial_data.rda
# consolidated_financial_data.rda
# revenue_data_member_ticker.rda
# bonds_ald_scenario.rda
# equity_ald_scenario.rda
# masterdata_ownership_datastore.rda
# masterdata_debt_datastore.rda
# debt_financial_data.rda

# Obtains data, processes the portfolio and saves the files

####################
#### DATA FILES ####
####################

fund_data <- c() # we dont need fund data, as we already have the EQ and CB input portfolios, we just want to merge the IPR/DNB/BOE sector classifications to the inputportfolios, through the financial data
fin_data <- get_and_clean_fin_data(fund_data = fund_data) %>%
  dplyr::select(isin, sector_boe, subsector_boe, sector_dnb, sector_ipr, subsector_ipr)

portfolio <- readRDS(file.path(project_location, "30_Processed_Inputs", paste0(project_name, "_total_portfolio.rda"))) %>%
  dplyr::left_join(fin_data, by = "isin") %>%
  dplyr::filter(investor_name == investor_name_filter)

portfolio_overview <- readRDS(file.path(project_location, "30_Processed_Inputs", paste0(project_name, "_overview_portfolio.rda"))) %>%
  dplyr::filter(valid_input == TRUE, asset_type %in% c("Equity", "Bonds"), investor_name == investor_name_filter) %>%
  dplyr::group_by(investor_name, portfolio_name, asset_type) %>%
  dplyr::summarise(
    portfolio_size = sum(valid_value_usd),
    .groups = "drop_last"
  )

# load portfolio results, get the plan_carsten values at start year, join in portfolio size and multiply with plan_carsten to get tech
cb_exposures <- readRDS(file.path(project_location, "40_Results", investor_name_filter, "Bonds_results_portfolio.rda")) %>%
  dplyr::filter(
    year == start_year,
    scenario_geography == "Global",
    equity_market %in% c("Global", "GlobalMarket")
  ) %>%
  dplyr::distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
  dplyr::inner_join(
    portfolio_overview %>% dplyr::filter(asset_type == "Bonds"),
    by = c("investor_name", "portfolio_name")
  ) %>%
  dplyr::mutate(tech_exposure = plan_carsten * portfolio_size)

eq_exposures <- readRDS(file.path(project_location, "40_Results", investor_name_filter, "Equity_results_portfolio.rda")) %>%
  dplyr::filter(
    year == start_year,
    scenario_geography == "Global",
    equity_market %in% c("Global", "GlobalMarket")
  ) %>%
  dplyr::distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
  dplyr::inner_join(
    portfolio_overview %>% dplyr::filter(asset_type == "Equity"),
    by = c("investor_name", "portfolio_name")
  ) %>%
  dplyr::mutate(tech_exposure = plan_carsten * portfolio_size)

calc_boe_exposures <- function(pacta_exposures) {
  pacta_exposures %>%
    dplyr::filter(ald_sector != "Other") %>%
    dplyr::mutate(
      subsector = dplyr::case_when(
        technology %in% c("RenewablesCap", "HydroCap", "NuclearCap") ~ "Low carbon",
        technology %in% c("ICE", "Hybrid", "FuelCell") ~ "Non electric",
        technology %in% c("Electric") ~ "Electric",
        technology %in% c("CoalCap") ~ "Coal Power",
        technology %in% c("GasCap") ~ "Gas Power",
        technology %in% c("OilCap") ~ "Oil Power",
        technology %in% c("Electric") ~ "Electric",

        ald_sector %in% c("Shipping", "Aviation") ~ NA_character_,
        ald_sector %in% c("Cement", "Steel") ~ "Fossil Fuel Based",
        TRUE ~ technology
      ),
      sector = dplyr::case_when(
        ald_sector %in% c("Oil&Gas", "Coal") ~ "Fuel extraction",
        ald_sector %in% c("Steel", "Cement") ~ "Materials",
        TRUE ~ ald_sector
      )
    ) %>%
    dplyr::group_by(investor_name, portfolio_name, sector, subsector) %>%
    dplyr::summarise(
      exposure = sum(tech_exposure, na.rm = T),
      .groups = "drop_last"
    )
}

boe_exposures_cb <- calc_boe_exposures(pacta_exposures = cb_exposures) %>%
  dplyr::mutate(asset_type = "Bonds")
boe_exposures_eq <- calc_boe_exposures(pacta_exposures = eq_exposures) %>%
  dplyr::mutate(asset_type = "Equity")


# Results -----------------------------------------------------------------

shocks <- readr::read_csv(file.path(data_location, "external_stress_test_shocks.csv"), col_types = "cccccn")

results_dnb <- portfolio %>%
  as.data.frame() %>%
  dplyr::filter(asset_type == "Equity") %>%
  dplyr::group_by(investor_name, portfolio_name, sector_dnb) %>%
  dplyr::summarise(
    exposure = sum(value_usd, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(sector = sector_dnb) %>%
  # ADO 1945 - left join ensures that all holdings that are not classified in
  # any DNB sector can be identifies as "Other"
  dplyr::left_join(
    shocks %>% dplyr::filter(methodology == "DNB") %>% dplyr::select(-c(methodology)),
    by = c("sector")
  ) %>%
  dplyr::mutate(
    loss = exposure * shock / 100,
    sector = ifelse(is.na(sector), "Other", sector)
  )

results_ipr <- portfolio %>%
  as.data.frame() %>%
  dplyr::filter(asset_type == "Equity") %>%
  dplyr::mutate(
    sector_ipr = ifelse(is.na(sector_ipr), "Other", sector_ipr),
    subsector_ipr = ifelse(is.na(subsector_ipr), "Other", subsector_ipr)
  ) %>%
  dplyr::group_by(investor_name, portfolio_name, sector_ipr, subsector_ipr) %>%
  dplyr::summarise(
    exposure = sum(value_usd, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(sector = sector_ipr, subsector = subsector_ipr) %>%
  # ADO 1945 - left join ensures that all holdings that are not classified in
  # any IPR sector can be identifies as "Other"
  dplyr::left_join(
    shocks %>% dplyr::filter(methodology == "IPR") %>% dplyr::select(-c(methodology)),
    by = c("sector", "subsector")
  ) %>%
  dplyr::mutate(
    loss = exposure * shock / 100,
    sector = ifelse(is.na(sector), "Other", sector)
  )

results_boe <- portfolio %>%
  as.data.frame() %>%
  dplyr::filter(
    asset_type %in% c("Equity", "Bonds"),
    sector_boe %in% c("Agriculture", "Food logistics", "Real estate", "Materials")
  ) %>%
  dplyr::group_by(investor_name, portfolio_name, sector_boe, subsector_boe, asset_type) %>%
  dplyr::summarise(
    exposure = sum(value_usd, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(sector = sector_boe, subsector = subsector_boe) %>%
  dplyr::bind_rows(boe_exposures_cb, boe_exposures_eq) %>%
  # ADO 1945 - left join ensures that all holdings that are not classified in
  # any IPR sector can be identifies as "Other"
  dplyr::left_join(
    shocks %>% dplyr::filter(methodology == "BoE") %>% dplyr::select(-c(description, methodology)),
    by = c("sector", "subsector")
  ) %>%
  dplyr::mutate(
    shock = ifelse(asset_type == "Bonds", 0.15 * shock, shock),
    loss = exposure * shock / 100,
    sector = ifelse(is.na(sector), "Other", sector)
  )

results_boe %>%
  readr::write_csv(file.path(results_path, investor_name_filter, "Stress_test_results_BOE.csv"))
results_ipr %>%
  readr::write_csv(file.path(results_path, investor_name_filter, "Stress_test_results_IPR.csv"))
results_dnb %>%
  readr::write_csv(file.path(results_path, investor_name_filter, "Stress_test_results_DNB.csv"))
