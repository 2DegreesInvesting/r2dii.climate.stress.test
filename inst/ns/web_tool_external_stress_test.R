# web tool version of external stress test scenario calculation - IPR, DNB, BoE

library(dplyr)
library(forcats)
library(fs)
library(fst)
library(ggplot2)
library(here)
library(jsonlite)
library(purrr)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)
library(zoo)


source(file.path(stress_test_path, "R", "get_st_data_path.R"))
source(file.path(stress_test_path, "R", "utils.R"))
source(file.path(stress_test_path, "R", "set_paths.R"))
source("0_web_functions.R") # This script is sourced from PACTA_analysis, so path is correct
source(file.path(stress_test_path, "stress_test_model_functions.R"))
source(file.path(stress_test_path, "0_global_functions_st.R"))
source(file.path(stress_test_path, "R", "set_params_st.R"))


devtools::load_all()

################
# INPUT VARIABLES
################

#### Project location----------------------------------------

data_location <- file.path(get_st_data_path(), data_path())

# Parameters passed from PACTA_analysis web_tool_script_2.R
pf_name <- portfolio_name_ref_all
investor_name_filter <- investor_name


####################
#### DATA FILES ####
####################

# NOTE: relevant fin data comes directly through the portfolio

# fund_data <- c() # we dont need fund data, as we already have the EQ and CB input portfolios, we just want to merge the IPR/DNB/BOE sector classifications to the inputportfolios, through the financial data
# fin_data <- get_and_clean_fin_data(fund_data) %>%
#   select(isin,sector_boe,subsector_boe,sector_dnb,sector_ipr,subsector_ipr)

if (file.exists(file.path(proc_input_path, pf_name, "total_portfolio.rda")) &
  file.exists(file.path(proc_input_path, pf_name, "overview_portfolio.rda"))) {
  # load total portfolio file - no need for joining fin_data
  portfolio <- read_rds(file.path(proc_input_path, pf_name, "total_portfolio.rda")) %>%
    dplyr::filter(investor_name == investor_name_filter)

  # load portfolio overview file
  portfolio_overview <- read_rds(file.path(proc_input_path, pf_name, "overview_portfolio.rda")) %>%
    dplyr::filter(valid_input == TRUE, asset_type %in% c("Equity", "Bonds"), investor_name == investor_name_filter) %>%
    group_by(investor_name, portfolio_name, asset_type) %>%
    dplyr::summarise(
      portfolio_size = sum(valid_value_usd),
      .groups = "drop_last"
    )
} else {
  print("Insufficient Portfolio Data available. Skipping IPR stress test!")
}


# load portfolio results, get the plan_carsten values at start year, join in portfolio size and multiply with plan_carsten to get tech
if (file.exists(file.path(results_path, pf_name, "Bonds_results_portfolio.rda"))) {
  cb_exposures <- read_rds(file.path(results_path, pf_name, "Bonds_results_portfolio.rda")) %>%
    dplyr::filter(year == start_year, scenario_geography == "Global", equity_market %in% c("Global", "GlobalMarket")) %>%
    dplyr::distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
    dplyr::inner_join(portfolio_overview %>%
                       dplyr::filter(asset_type == "Equity"), by = c("investor_name", "portfolio_name")) %>%
    dplyr::mutate(tech_exposure = plan_carsten * portfolio_size)
} else {
  print("No Bonds Portfolio Data available. Skipping!")
}

if (file.exists(file.path(results_path, pf_name, "Equity_results_portfolio.rda"))) {
  eq_exposures <- read_rds(file.path(results_path, pf_name, "Equity_results_portfolio.rda")) %>%
    dplyr::filter(year == start_year, scenario_geography == "Global", equity_market %in% c("Global", "GlobalMarket")) %>%
    dplyr::distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
    dplyr::inner_join(portfolio_overview %>%
                       dplyr::filter(asset_type == "Equity"), by = c("investor_name", "portfolio_name")) %>%
    dplyr::mutate(tech_exposure = plan_carsten * portfolio_size)
} else {
  print("No Equity Portfolio Data available. Skipping!")
}

# load external shock data
shocks <-
  readr::read_csv(file.path(data_location, "external_stress_test_shocks.csv"),
    na = c("", "NA", "#VALUE!"), col_types = cols()
  )

# Calculate Results -----------------------------------------------------------------

# FIXME: If result is empty, what to return?
# calculate IPR results
if (exists("portfolio")) {
  results_ipr <- portfolio %>%
    as.data.frame() %>%
    dplyr::filter(asset_type == "Equity") %>%
    group_by(investor_name, portfolio_name, sector_ipr, subsector_ipr) %>%
    dplyr::summarise(
      exposure = sum(value_usd, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
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
    ) %>%
    dplyr::ungroup()
} else {
  print("No portfolio data -- skipping IPR stress test")
}


# Write Results -----------------------------------------------------------------

if (exists("results_ipr")) {
  if (any(unique(results_ipr$sector) != "Other")) {
    results_ipr %>% readr::write_rds(file.path(results_path, pf_name, "Stress_test_results_IPR.rds"))
  }
} else {
  "No Stress Test results available for IPR FPS Scenario"
}
