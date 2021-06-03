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


source(file.path(stress_test_path, "R", "utils.R"))
source(file.path(stress_test_path, "R", "set_paths.R"))
source("0_web_functions.R") # This script is sourced from PACTA_analysis, so path is correct
source(file.path(stress_test_path, "stress_test_model_functions.R"))
source(file.path(stress_test_path, "0_global_functions_st.R"))
source(file.path(stress_test_path, "R", "set_params_st.R"))


devtools::load_all()
setup_project()

################
# INPUT VARIABLES
################

#### Project location----------------------------------------

# TODO: ensure this is updated to point to r2dii.stress.test.data
# TODO: stress_test_path probably won't work anymore for loading data. Replace!
data_location <- file.path(stress_test_path, data_path())

# Parameters passed from PACTA_analysis web_tool_script_2.R
pf_name <- portfolio_name_ref_all
investor_name_filter <- investor_name


#### Analysis Parameters----------------------------------------
# Get analysis parameters from the projects AnalysisParameters.yml - similar to PACTA_analysis
set_project_parameters(file.path(working_location, "parameter_files",paste0("ProjectParameters_", project_code, ".yml")))

# This sets the file location, should work with data_location_ext from setup_project(). otherwise use analysis_inputs_path
# analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)
file_location <- file.path(data_location_ext, "cleaned_files")



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
    filter(investor_name == investor_name_filter)

  # load portfolio overview file
  portfolio_overview <- read_rds(file.path(proc_input_path, pf_name, "overview_portfolio.rda")) %>%
    filter(valid_input == TRUE, asset_type %in% c("Equity", "Bonds"), investor_name == investor_name_filter) %>%
    group_by(investor_name, portfolio_name, asset_type) %>%
    summarise(
      portfolio_size = sum(valid_value_usd),
      .groups = "drop_last"
    )
} else {
  print("Insufficient Portfolio Data available. Skipping IPR stress test!")
}



# load portfolio results, get the plan_carsten values at start year, join in portfolio size and multiply with plan_carsten to get tech
if (file.exists(file.path(results_path, pf_name, "Bonds_results_portfolio.rda"))) {
  cb_exposures <- read_rds(file.path(results_path, pf_name, "Bonds_results_portfolio.rda")) %>%
    filter(year == start_year, scenario_geography == "Global", equity_market %in% c("Global", "GlobalMarket")) %>%
    distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
    left_join(portfolio_overview %>%
      filter(asset_type == "Equity"), by = c("investor_name", "portfolio_name")) %>%
    mutate(tech_exposure = plan_carsten * portfolio_size)
} else {
  print("No Bonds Portfolio Data available. Skipping!")
}

if (file.exists(file.path(results_path, pf_name, "Equity_results_portfolio.rda"))) {
  eq_exposures <- read_rds(file.path(results_path, pf_name, "Equity_results_portfolio.rda")) %>%
    filter(year == start_year, scenario_geography == "Global", equity_market %in% c("Global", "GlobalMarket")) %>%
    distinct(investor_name, portfolio_name, ald_sector, technology, plan_carsten, plan_sec_carsten) %>%
    left_join(portfolio_overview %>%
      filter(asset_type == "Equity"), by = c("investor_name", "portfolio_name")) %>%
    mutate(tech_exposure = plan_carsten * portfolio_size)
} else {
  print("No Equity Portfolio Data available. Skipping!")
}

# load external shock data
shocks <-
  readr::read_csv(file.path(stress_test_path, data_path("external_stress_test_shocks.csv")),
    na = c("", "NA", "#VALUE!"), col_types = cols()
  )




# Calculate Results -----------------------------------------------------------------

# NOTE: Currently we only calculate and return IPR results in the webtool. BoE and DNB are comented out, but might be added again soon

# # calculate BoE exposures
# calc_boe_exposures <- function(pacta_exposures){
#   pacta_exposures %>%
#     filter(ald_sector!='Other') %>%
#     mutate(
#       subsector = case_when(
#         technology %in% c('RenewablesCap','HydroCap','NuclearCap') ~ "Low carbon",
#         technology %in% c('ICE','Hybrid','FuelCell') ~ "Non electric",
#         technology %in% c('Electric') ~ "Electric",
#         technology %in% c('CoalCap') ~ "Coal Power",
#         technology %in% c('GasCap') ~ "Gas Power",
#         technology %in% c('OilCap') ~ "Oil Power",
#         technology %in% c('Electric') ~ "Electric",
#
#         ald_sector %in% c('Shipping','Aviation') ~ NA_character_,
#         ald_sector %in% c('Cement','Steel') ~ "Fossil Fuel Based",
#         TRUE ~ technology
#       ),
#       sector = case_when(
#         ald_sector %in% c('Oil&Gas','Coal') ~ "Fuel extraction",
#         ald_sector %in% c('Steel','Cement') ~ "Materials",
#         TRUE ~ ald_sector
#       )) %>%
#     group_by(investor_name,portfolio_name,sector,subsector) %>%
#     summarise(exposure = sum(tech_exposure,na.rm=T),
#               .groups = "drop_last")
# }
#
# #FIXME: Produce empty tibble if one of the asset types is missing
# if(exists("cb_exposures")){boe_exposures_cb <- calc_boe_exposures(cb_exposures) %>% mutate(asset_type='Bonds')}
# if(exists("eq_exposures")){boe_exposures_eq <- calc_boe_exposures(eq_exposures) %>% mutate(asset_type='Equity')}
#
# # calculate BoE results
# results_boe <- portfolio %>%
#   as.data.frame() %>%
#   filter(asset_type %in% c('Equity','Bonds'),sector_boe %in% c('Agriculture','Food logistics','Real estate','Materials')) %>%
#   group_by(investor_name,portfolio_name,sector_boe,subsector_boe,asset_type) %>%
#   summarise(exposure = sum(value_usd,na.rm=TRUE),
#             .groups = "drop_last") %>%
#   rename(sector=sector_boe,subsector=subsector_boe) %>%
#   bind_rows(boe_exposures_cb,boe_exposures_eq) %>%
#   left_join(shocks %>% filter(methodology=='BoE') %>% select(-c(description,methodology)),by=c('sector','subsector')) %>%
#   mutate(shock=ifelse(asset_type=='Bonds',0.15*shock,shock),
#          loss = exposure * shock/100)
#
# #FIXME: If result is empty, what to return?
# # calculate DNB results
# results_dnb <- portfolio %>%
#   as.data.frame() %>%
#   filter(asset_type=='Equity') %>%
#   group_by(investor_name,portfolio_name,sector_dnb) %>%
#   summarise(exposure = sum(value_usd,na.rm=TRUE),
#             .groups = "drop_last") %>%
#   rename(sector = sector_dnb) %>%
#   left_join(shocks %>% filter(methodology=='DNB') %>% select(-c(methodology)),by=c('sector')) %>%
#   mutate(loss = exposure * shock/100)

# FIXME: If result is empty, what to return?
# calculate IPR results
if (exists("portfolio")) {
  results_ipr <- portfolio %>%
    as.data.frame() %>%
    filter(asset_type == "Equity") %>%
    group_by(investor_name, portfolio_name, sector_ipr, subsector_ipr) %>%
    summarise(
      exposure = sum(value_usd, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    rename(sector = sector_ipr, subsector = subsector_ipr) %>%
    left_join(
      shocks %>% filter(methodology == "IPR") %>% select(-c(methodology)),
      by = c("sector", "subsector")
    ) %>%
    mutate(
      loss = exposure * shock / 100,
      sector = ifelse(is.na(sector), "Other", sector)
    ) %>%
    ungroup()
} else {
  print("No portfolio data -- skipping IPR stress test")
}


# Write Results -----------------------------------------------------------------

# NOTE: Currently we only calculate and return IPR results in the webtool. BoE and DNB are comented out, but might be added again soon

# if(exists("results_boe")){
#   results_boe %>% readr::write_csv(file.path(results_path,pf_name,"Stress_test_results_BOE.csv"))
# } else {"No Stress Test results available for BoE Scenarios"}
# if(exists("results_dnb")){
#   results_dnb %>% readr::write_csv(file.path(results_path,pf_name,"Stress_test_results_DNB.csv"))
# } else {"No Stress Test results available for DNB Scenarios"}
if (exists("results_ipr") &
    any(unique(results_ipr$sector) != "Other")) {
  results_ipr %>% readr::write_rds(file.path(results_path, pf_name, "Stress_test_results_IPR.rds"))
} else {
  "No Stress Test results available for IPR FPS Scenario"
}
