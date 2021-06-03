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
library(tidyselect)
library(tidyr)

source(file.path("R","utils.R"))
source(file.path("R","set_paths.R"))
source(file.path("R","get_st_data_path.R"))
source("stress_test_model_functions.R")
source("0_global_functions_st.R")
source("0_portfolio_input_check_functions.R")

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
check_valid_cfg(cfg_st, expected_no_args = 3)
project_name <- cfg_st$project_name
twodii_internal <- cfg_st$project_internal$twodii_internal
project_location_ext <- cfg_st$project_internal$project_location_ext
data_location_ext <- cfg_st$project_internal$data_location_ext
price_data_version <- cfg_st$price_data_version

data_location <- file.path(get_st_data_path(), data_path())

data_location <- ifelse(
  twodii_internal == TRUE,
  data_location,
  data_location_ext
)

set_project_paths(project_name, twodii_internal, project_location_ext)
# TODO: Remove dead code? The option 'r2dii_config' seems unused
options(r2dii_config = file.path(par_file_path, "AnalysisParameters.yml"))

set_global_parameters(file.path(par_file_path, "AnalysisParameters.yml"))

analysis_inputs_path <- set_analysis_inputs_path(
  twodii_internal = twodii_internal,
  data_location_ext = data_location_ext,
  dataprep_ref = dataprep_timestamp
)


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
currencies <- get_and_clean_currency_data()

fund_data <- get_and_clean_fund_data()

fin_data <- get_and_clean_fin_data(fund_data = fund_data)

comp_fin_data <- get_and_clean_company_fin_data()

debt_fin_data <- get_and_clean_debt_fin_data()

####################
#### PORTFOLIOS ####
####################
portfolio_raw <- read_raw_portfolio_file(project_name = project_name)

portfolio <- ungroup(portfolio_raw) %>%
  process_raw_portfolio(
    fin_data = fin_data,
    fund_data = fund_data,
    currencies = currencies,
    grouping_variables = grouping_variables
  )

eq_portfolio <- create_portfolio_subset(
  portfolio = portfolio %>% ungroup(),
  portfolio_type = "Equity",
  relevant_fin_data = comp_fin_data
)

cb_portfolio <- create_portfolio_subset(
  portfolio = portfolio %>% ungroup(),
  portfolio_type = "Bonds",
  relevant_fin_data = debt_fin_data
)

# portfolio_total <- add_portfolio_flags(portfolio %>% ungroup())

# portfolio_overview <- portfolio_summary(portfolio_total)

masterdata_debt <- readRDS(file.path(analysis_inputs_path, "masterdata_debt_datastore.rda")) %>%
  select(id, id_name, ald_sector, technology, ald_production, year)
masterdata_equity <- readRDS(file.path(analysis_inputs_path, "masterdata_ownership_datastore.rda")) %>%
  select(id, id_name, ald_sector, technology, ald_production, year)

calc_pacta_exposures <- function(masterdata, portfolio) {
  company_tech_shares <- masterdata %>%
    filter(year == start_year) %>%
    group_by(id, id_name, ald_sector, technology) %>%
    summarise(tech_production = sum(ald_production), .groups = "drop_last") %>%
    group_by(id, id_name, ald_sector) %>%
    mutate(company_production = sum(tech_production, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(tech_share = tech_production / company_production) %>%
    select(id, ald_sector, technology, tech_share) %>%
    rename(financial_sector = ald_sector)

  company_tech_split <- portfolio %>%
    group_by(investor_name, portfolio_name, company_id, id, id_name, financial_sector) %>%
    summarise(value_usd = sum(value_usd), .groups = "drop_last") %>%
    left_join(company_tech_shares, by = c("id", "financial_sector"))

  average_tech_shares <- company_tech_split %>%
    filter(!is.na(technology)) %>%
    group_by(investor_name, portfolio_name, financial_sector, technology) %>%
    summarise(tech_exposure = sum(value_usd), .groups = "drop_last") %>%
    group_by(investor_name, portfolio_name, financial_sector) %>%
    mutate(sector_exposure = sum(tech_exposure)) %>%
    mutate(tech_share = tech_exposure / sector_exposure) %>%
    select(-c(tech_exposure, sector_exposure))

  tech_split_missing_companies <- company_tech_split %>%
    filter(is.na(tech_share)) %>%
    select(-c(technology, tech_share)) %>%
    left_join(average_tech_shares, by = c("investor_name", "portfolio_name", "financial_sector"))

  company_tech_split <- bind_rows(company_tech_split %>% filter(!is.na(technology)), tech_split_missing_companies)

  company_tech_split %>%
    group_by(investor_name, portfolio_name, financial_sector, technology) %>%
    summarise(exposure = sum(value_usd * tech_share), .groups = "drop_last")
}

pacta_exposures_cb <- calc_pacta_exposures(
  masterdata = masterdata_debt,
  portfolio = cb_portfolio
)
pacta_exposures_eq <- calc_pacta_exposures(
  masterdata = masterdata_equity,
  portfolio = eq_portfolio
)

calc_boe_exposures <- function(pacta_exposures) {
  pacta_exposures %>%
    filter(financial_sector != "Other") %>%
    mutate(
      subsector = case_when(
        technology %in% c("RenewablesCap", "HydroCap", "NuclearCap") ~ "Low carbon",
        technology %in% c("ICE", "Hybrid", "FuelCell") ~ "Non electric",
        technology %in% c("Electric") ~ "Electric",
        technology %in% c("CoalCap") ~ "Coal Power",
        technology %in% c("GasCap") ~ "Gas Power",
        technology %in% c("OilCap") ~ "Oil Power",
        technology %in% c("Electric") ~ "Electric",

        financial_sector %in% c("Shipping", "Aviation") ~ NA_character_,
        financial_sector %in% c("Cement", "Steel") ~ "Fossil Fuel Based",
        TRUE ~ technology
      ),
      sector = case_when(
        financial_sector %in% c("Oil&Gas", "Coal") ~ "Fuel extraction",
        financial_sector %in% c("Steel", "Cement") ~ "Materials",
        TRUE ~ financial_sector
      )
    ) %>%
    group_by(investor_name, portfolio_name, sector, subsector) %>%
    summarise(exposure = sum(exposure, na.rm = TRUE), .groups = "drop_last")
}

boe_exposures_cb <- calc_boe_exposures(pacta_exposures = pacta_exposures_cb) %>%
  mutate(asset_type = "Bonds")
boe_exposures_eq <- calc_boe_exposures(pacta_exposures = pacta_exposures_eq) %>%
  mutate(asset_type = "Equity")


# Results -----------------------------------------------------------------

shocks <- readr::read_csv(file.path(data_location, "external_stress_test_shocks.csv"), col_types = "cccccn")

results_dnb <- portfolio %>%
  as.data.frame() %>%
  filter(asset_type == "Equity") %>%
  group_by(investor_name, portfolio_name, sector_dnb) %>%
  summarise(exposure = sum(value_usd, na.rm = TRUE), .groups = "drop_last") %>%
  rename(sector = sector_dnb) %>%
  left_join(
    shocks %>% filter(methodology == "DNB") %>% select(-c(methodology)),
    by = c("sector")
  ) %>%
  mutate(loss = exposure * shock / 100)

results_ipr <- portfolio %>%
  as.data.frame() %>%
  filter(asset_type == "Equity") %>%
  group_by(investor_name, portfolio_name, sector_ipr, subsector_ipr) %>%
  summarise(exposure = sum(value_usd, na.rm = TRUE), .groups = "drop_last") %>%
  rename(sector = sector_ipr, subsector = subsector_ipr) %>%
  left_join(
    shocks %>% filter(methodology == "IPR") %>% select(-c(methodology)),
    by = c("sector", "subsector")
  ) %>%
  mutate(
    loss = exposure * shock / 100,
    sector = ifelse(is.na(sector), "Other", sector)
  )

results_boe <- portfolio %>%
  as.data.frame() %>%
  filter(asset_type %in% c("Equity", "Bonds"), sector_boe %in% c("Agriculture", "Food logistics", "Real estate", "Materials")) %>%
  group_by(investor_name, portfolio_name, sector_boe, subsector_boe, asset_type) %>%
  summarise(exposure = sum(value_usd, na.rm = TRUE), .groups = "drop_last") %>%
  rename(sector = sector_boe, subsector = subsector_boe) %>%
  bind_rows(boe_exposures_cb, boe_exposures_eq) %>%
  left_join(
    shocks %>% filter(methodology == "BoE") %>% select(-c(description, methodology)),
    by = c("sector", "subsector")
  ) %>%
  mutate(
    shock = ifelse(asset_type == "Bonds", 0.15 * shock, shock),
    loss = exposure * shock / 100
  )

readr::write_csv(results_boe, file.path(results_path,"st_results_boe.csv"))
readr::write_csv(results_ipr, file.path(results_path,"st_results_ipr.csv"))
readr::write_csv(results_dnb, file.path(results_path,"st_results_dnb.csv"))
