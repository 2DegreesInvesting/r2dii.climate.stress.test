library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(purrr)
library(readr)
library(forcats)
library(readxl)
library(tidyselect)
library(fs)
library(jsonlite)
library(fst)
library(here)
library(tibble)
library(stringr)
library(zoo)

source(file.path(stress_test_path, "R", "functions.R"))

function_paths <- c(
  "0_web_functions.R", # This script is sourced from PACTA_analysis, so path is correct
  file.path(
    stress_test_path,
    "R",
    c(
      "apply_filters.R",
      "asset_value_at_risk.R",
      "check_and_filter_data.R",
      "company_asset_value_at_risk.R",
      "convert_cap_to_generation.R",
      "extend_scenario_trajectory.R",
      "get_st_data_path.R",
      "interpolate_automotive_scenario.R",
      "read_capacity_factors.R",
      "read_transition_scenarios.R",
      "read_pacta_results.R",
      "read_price_data.R",
      "set_paths.R",
      "set_tech_trajectories.R",
      "stress_test_model_functions.R",
      "utils.R",
      "write_results.R"
    )
  )
)


source_all(function_paths)

devtools::load_all()

################
# INPUT VARIABLES
################

#### Project location----------------------------------------

data_location <- file.path(get_st_data_path(), data_path())

# Parameters passed from PACTA_analysis web_tool_script_2.R
pf_name <- portfolio_name_ref_all
# investor_name is passed via GlobalEnv of P2020 webtool
# project_code is passed via GlobalEnv of P2020 webtool
# price_data_version passed via GlobalEnv of P2020 webtool; fallback use "2020Q4" if old parameter file used in webtool
price_data_version <- if (exists("price_data_version")) price_data_version else "2020Q4"
calculation_level <- if (exists("calculation_level")) calculation_level else "portfolio"
company_exclusion <- if (exists("company_exclusion")) company_exclusion else FALSE

##### Filters----------------------------------------
# The filter settings should comply with the filters from the parent PACTA project as per default
# There may still be cases of certain sectors or geographies that work in PACTA but not yet in stress testing
# move to config once mechanism to include/exclude filters from original pacta project exists

#### OPEN: This could largely be taken from cfg file. No apparent reason why not.
scenario_geography_filter <- "Global"
# scenario_geography_filter <- cfg$Lists$Scenario.Geography.List

# ALLOW ONLY precisely the scenarios that are supposed to be kept from the portfolio and scen_data
# NOTE scenarios from the same source, same secenario name and diff years will likely fail
# E.g. WEO2019_SDS AND WEO2020_SDS will produce near-duplicates that break the analysis
scenarios <- c(
  # "B2DS",
  # "CPS",#"NPS","NPSRTS","SDS",)#,
  "ETP2017_B2DS",
  "ETP2017_NPS",
  "ETP2017_SDS",
  "WEO2019_CPS",
  "WEO2019_NPS",
  "WEO2019_SDS" # ,
  # "WEO2020_NPS",
  # "WEO2020_SDS"
)
# scenarios <- cfg$Large.Universe.Filter$SCENARIO.FILTER



allocation_method_equity <- "portfolio_weight"
# In P2020, only allow global market Stress Test.
# Functionality needs to be refined, to run multiple simultaneously
# equity_market_filter <- cfg$Lists$Equity.Market.List
equity_market_filter <- c("GlobalMarket", "Global")

sectors <- c("Power", "Oil&Gas", "Coal", "Automotive")
# setors <- cfg$Large.Universe.Filter$SECTOR.FILTER
technologies <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)
# technologies <- cfg$Lists$Technology.List

#### Model variables----------------------------------------
#### OPEN: This should be moved into a StressTestModelParameters.yml
cfg_mod <- config::get(file = file.path(stress_test_path, "model_parameters", paste0("model_parameters_", project_code, ".yml")))

# OPEN: wrap reading in of params in function and move to global_functions
end_year <- cfg_mod$end_year # Set to 2040 cause current scenario data goes until 2040. can be extended when WEO2020 turns out extended horizon

# Scenarios in the model_parameters.yml file must have the short names (SDS, NPS, etc)
scenario_to_follow_baseline <- cfg_mod$scenarios$scenario_to_follow_baseline # sets which scenario trajectory the baseline scenario follows
scenario_to_follow_ls <- cfg_mod$scenarios$scenario_to_follow_ls # sets which scenario trajectory LS scenario follows after shock period
scenario_to_follow_ls_aligned <- cfg_mod$scenarios$scenario_to_follow_ls_aligned

scenarios_filter <- unique(
  c(
    scenario_to_follow_baseline,
    scenario_to_follow_ls,
    scenario_to_follow_ls_aligned
  )
)

discount_rate <- cfg_mod$financials$discount_rate # Discount rate
##### OPEN: this needs to be estimated based on data
terminal_value <- cfg_mod$financials$terminal_value
div_netprofit_prop_coef <- cfg_mod$financials$div_netprofit_prop_coef # determine this value using bloomberg data

# technology net profit margins
## the parameters should be outsorced into a config file at some point
## they should also be looped over, if multiple scenarios are to be analysed
net_profit_margin_coal <- cfg_mod$net_profit_margin$coal
net_profit_margin_coalcap <- cfg_mod$net_profit_margin$coalcap
net_profit_margin_electric <- cfg_mod$net_profit_margin$electric
net_profit_margin_gas <- cfg_mod$net_profit_margin$gas
net_profit_margin_gascap <- cfg_mod$net_profit_margin$gascap
net_profit_margin_hybrid <- cfg_mod$net_profit_margin$hybrid
net_profit_margin_ice <- cfg_mod$net_profit_margin$ice
net_profit_margin_nuclearcap <- cfg_mod$net_profit_margin$nuclearcap
net_profit_margin_oil <- cfg_mod$net_profit_margin$oil
net_profit_margin_oilcap <- cfg_mod$net_profit_margin$oilcap
net_profit_margin_renewablescap <- cfg_mod$net_profit_margin$renewablescap
net_profit_margin_hydrocap <- cfg_mod$net_profit_margin$hydrocap



###############
# Load input datasets----------------------------------------

sector_exposures <- readRDS(file.path(proc_input_path, pf_name, "overview_portfolio.rda"))

# Load transition scenarios that will be run by the model
transition_scenarios <- read_transition_scenarios(
  path = file.path(data_location, "project_transition_scenarios", glue::glue("transition_scenario_{project_code}.csv")),
  start_of_analysis = start_year,
  end_of_analysis = end_year
)
# TODO: remove once this can be tested within webtool
# transition_scenarios <- readr::read_csv(file.path(stress_test_path, data_path("project_transition_scenarios", paste0("transition_scenario_", project_code, ".csv"))), col_types = cols()) %>%
#   mutate(
#     overshoot_method = ifelse(is.na(overshoot_method), FALSE, overshoot_method),
#     duration_of_shock = ifelse(overshoot_method, end_year - year_of_shock + 1, duration_of_shock)
#   ) %>%
#   check_scenario_consistency()

# Load utilization factors power
# TODO: replace with new capacity factors
capacity_factors_power <- read_capacity_factors_old(
  path = file.path(data_location, "capacity_factors_WEO_2017.csv")
)

# Load scenario data----------------------------------------

scen_data_file <- ifelse(twodii_internal == TRUE,
  path_dropbox_2dii("PortCheck", "00_Data", "01_ProcessedData", "03_ScenarioData", paste0("Scenarios_AnalysisInput_", start_year, ".csv")),
  file.path(data_location, glue::glue("Scenarios_AnalysisInput_{start_year}.csv"))
)

scenario_data <- readr::read_csv(scen_data_file, col_types = cols(.default = col_guess())) %>%
  dplyr::rename(source = scenario_source) %>%
  dplyr::filter(source %in% c("ETP2017", "WEO2019")) %>%
  dplyr::mutate(scenario = ifelse(stringr::str_detect(scenario, "_"), stringr::str_extract(scenario, "[^_]*$"), scenario)) %>%
  check_scenario_timeframe(start_year = start_year, end_year = end_year)


# Correct for automotive scenario data error. CHECK IF ALREADY RESOLVED IN THE SCENARIO DATA, IF SO, DONT USE FUNCTION BELOW!
scenario_data <- scenario_data %>%
  correct_automotive_scendata(interpolation_years = c(2031:2034, 2036:2039))

# %>% filter(year %in% c(start_year,2020, 2021, 2022, 2023, 2024, 2025, 2030, 2035, 2040))

df_price <- read_price_data_old(
    path = file.path(data_location, paste0("prices_data_", price_data_version, ".csv")),
    expected_technologies = technologies
  ) %>%
  dplyr::filter(year >= start_year) %>%
  check_price_consistency(start_year = start_year)


#############
# Create shock net profits margins dataframe

net_profit_margins <- net_profit_margin_setup(
  net_profit_margin_coal = net_profit_margin_coal,
  net_profit_margin_coalcap = net_profit_margin_coalcap,
  net_profit_margin_electric = net_profit_margin_electric,
  net_profit_margin_gas = net_profit_margin_gas,
  net_profit_margin_gascap = net_profit_margin_gascap,
  net_profit_margin_hybrid = net_profit_margin_hybrid,
  net_profit_margin_ice = net_profit_margin_ice,
  net_profit_margin_nuclearcap = net_profit_margin_nuclearcap,
  net_profit_margin_oil = net_profit_margin_oil,
  net_profit_margin_renewablescap = net_profit_margin_renewablescap,
  net_profit_margin_hydrocap = net_profit_margin_hydrocap,
  net_profit_margin_oilcap = net_profit_margin_oilcap
)

if (identical(calculation_level, "company") & company_exclusion) {
  excluded_companies <- readr::read_csv(
    file.path(data_location, "exclude-companies.csv"),
    col_types = "cc"
  )
}


###############
# Prepare data for stress test model----------------------------------------
###############

# OPEN:: IMPORTANT: ADD input data consistency checks. E.G. the start year of the iput data must be the same as the start year set in the project parameters

###############
# Create input data for stress test model----------------------------------------
###############

## if we use the integral/overshoot late&sudden method, and we use company production plans the first 5 years
## the integral method works on company level, however,
## when we aggregate the company LS trajectories to port-technology level, the integrals of SDS and LS are not the same, due to 2 reasons:
## 1) for companies that outperform SDS, capacity shhould not be compensated for, hence we take a LS trajectory that equal SDS
## 2) there are cases for which the linear compensation is so strong, that the LS production falls below zero, which is then set to zero (as negative production is not possible), hence we have an underestimation in overshoot
## For these two reasons, if we use company production plans, we perform the integral method on technology level (and not on company level), until we had a proper session on how to deal with these issues

nesting_vars <- c(
  "investor_name", "portfolio_name", "equity_market", "ald_sector", "technology",
  "scenario", "allocation", "scenario_geography"#, "company_name"
)
if (identical(calculation_level, "company")) {nesting_vars <- c(nesting_vars, "company_name")}

# the webtool should run through regardless of whether there are data for only one of the asset types or both
if (file.exists(file.path(results_path, pf_name, paste0("Equity_results_", calculation_level, ".rda")))) {
  print("Calculate Stress Test for Equity Portfolio")

  equity_path <- file.path(results_path, pf_name, paste0("Equity_results_", calculation_level, ".rda"))

  pacta_equity_results_full <- read_pacta_results(
    path = equity_path,
    level = calculation_level
  )

  pacta_equity_results_full <- pacta_equity_results_full %>%
    dplyr::filter(!(scenario == "ETP2017_NPS" & ald_sector == "Power")) %>%
    # ADO 2473 - hotfix - ensure scenario names match across data sets (SPS is the follow up to NPS)
    dplyr::mutate(scenario = dplyr::if_else(stringr::str_detect(scenario, "WEO2019_SPS"), "WEO2019_NPS", scenario)) %>%
    dplyr::filter(scenario %in% scenarios) %>%
    dplyr::mutate(scenario = ifelse(stringr::str_detect(scenario, "_"), stringr::str_extract(scenario, "[^_]*$"), scenario)) %>%
    check_portfolio_consistency(start_year = start_year)

  pacta_equity_results <- pacta_equity_results_full %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      nesting(!!!syms(nesting_vars))
    ) %>%
    dplyr::mutate(plan_tech_prod = dplyr::if_else(is.na(plan_tech_prod), 0, plan_tech_prod)) %>%
    apply_filters(
      investor = investor_name,
      sectors = sectors,
      technologies = technologies,
      scenario_geography_filter = scenario_geography_filter,
      scenarios = scenarios_filter,
      allocation_method = allocation_method_equity,
      start_analysis = start_year
    ) %>%
    dplyr::filter(
      allocation == allocation_method_equity,
      equity_market %in% equity_market_filter
    ) %>%
    dplyr::mutate(scenario = stringr::str_replace(scenario, "NPSRTS", "NPS")) %>%
    dplyr::distinct_all()

  if (nrow(pacta_equity_results) <= 0) {
    print("Input pacta data has 0 valid rows after filtering. Skipping equity calculation!")
  } else {

    # check scenario availability across data inputs for equity
    check_scenario_availability(
      portfolio = pacta_equity_results,
      scen_data = scenario_data,
      scenarios = scenarios_filter
    )

    equity_port_aum <- sector_exposures %>%
      dplyr::filter(asset_type == "Equity") %>%
      dplyr::group_by(investor_name, portfolio_name) %>%
      dplyr::summarise(
        asset_portfolio_value = sum(valid_value_usd),
        .groups = "drop_last"
      )

    # Equity results  ------------------------------------------------------------------

    equity_results <- c()

    for (i in seq(1, nrow(transition_scenarios))) {
      transition_scenario_i <- transition_scenarios[i, ]

      # Calculate late and sudden prices for scenario i
      df_prices <- df_price %>%
        dplyr::mutate(Baseline = NPS) %>% # FIXME this should be parameterized!!
        dplyr::rename(
          year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
          SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
        ) %>%
        dplyr::group_by(ald_sector, technology) %>%
        #### OPEN: Potentially a problem with the LS price calculation. Concerning warning
        dplyr::mutate(
          late_sudden_price = late_sudden_prices(
            SDS_price = SDS_price,
            Baseline_price = Baseline_price,
            year_of_shock = transition_scenario_i$year_of_shock,
            start_year = start_year,
            duration_of_shock = transition_scenario_i$duration_of_shock
          )
        ) %>%
        dplyr::ungroup()

      # Convert capacity (MW)to generation (MWh) for power sector
      equity_annual_profits <- pacta_equity_results %>%
        convert_cap_to_generation(capacity_factors_power = capacity_factors_power) %>%
        extend_scenario_trajectory(
          scenario_data = scenario_data,
          start_analysis = start_year,
          end_analysis = end_year,
          time_frame = time_horizon
        ) %>%
        set_baseline_trajectory(
          scenario_to_follow_baseline = scenario_to_follow_baseline
        ) %>%
        set_ls_trajectory(
          scenario_to_follow_ls = scenario_to_follow_ls,
          shock_scenario = transition_scenario_i,
          scenario_to_follow_ls_aligned = scenario_to_follow_ls_aligned,
          start_year = start_year,
          end_year = end_year,
          analysis_time_frame = time_horizon
        )

      if (exists("excluded_companies")) {
        equity_annual_profits <- equity_annual_profits %>%
          exclude_companies(
            exclusion = excluded_companies,
            scenario_baseline = scenario_to_follow_baseline,
            scenario_ls = scenario_to_follow_ls
          )
      }

      equity_annual_profits <- equity_annual_profits %>%
        join_price_data(df_prices = df_prices) %>%
        join_net_profit_margins(net_profit_margins = net_profit_margins) %>%
        calculate_net_profits() %>%
        dcf_model_techlevel(discount_rate = discount_rate)

      plan_carsten_equity <- pacta_equity_results %>%
        dplyr::filter(
          year == start_year,
          technology %in% technologies,
          scenario_geography == scenario_geography_filter
        )

      if(identical(calculation_level, "company")) {
        plan_carsten_equity <- plan_carsten_equity %>%
          dplyr::distinct(investor_name, portfolio_name, company_name, ald_sector, technology,
                   scenario_geography, year, plan_carsten, plan_sec_carsten)

        if (!exists("excluded_companies")) {
          equity_results <- dplyr::bind_rows(
            equity_results,
            company_asset_value_at_risk(
              data = equity_annual_profits,
              terminal_value = terminal_value,
              shock_scenario = transition_scenario_i,
              div_netprofit_prop_coef = div_netprofit_prop_coef,
              plan_carsten = plan_carsten_equity,
              port_aum = equity_port_aum,
              flat_multiplier = 1,
              exclusion = NULL
            )
          )
        } else {
          equity_results <- dplyr::bind_rows(
            equity_results,
            company_asset_value_at_risk(
              data = equity_annual_profits,
              terminal_value = terminal_value,
              shock_scenario = transition_scenario_i,
              div_netprofit_prop_coef = div_netprofit_prop_coef,
              plan_carsten = plan_carsten_equity,
              port_aum = equity_port_aum,
              flat_multiplier = 1,
              exclusion = excluded_companies
            )
          )
        }

      } else {
        plan_carsten_equity <- plan_carsten_equity %>%
          dplyr::distinct(investor_name, portfolio_name, ald_sector, technology,
                   scenario_geography, year, plan_carsten, plan_sec_carsten)

        equity_results <- dplyr::bind_rows(
          equity_results,
          asset_value_at_risk(
            data = equity_annual_profits,
            terminal_value = terminal_value,
            shock_scenario = transition_scenario_i,
            div_netprofit_prop_coef = div_netprofit_prop_coef,
            plan_carsten = plan_carsten_equity,
            port_aum = equity_port_aum,
            flat_multiplier = 1
          )
        )
      }

    }
  }
} else {
  print("No Equity Portfolio Data available. Skipping!")
}


if (file.exists(file.path(results_path, pf_name, paste0("Bonds_results_", calculation_level, ".rda")))) {
  print("Calculate Stress Test for Bonds Portfolio")

  bonds_path <- file.path(results_path, pf_name, paste0("Bonds_results_", calculation_level, ".rda"))

  pacta_bonds_results_full <- read_pacta_results(
    path = bonds_path,
    level = calculation_level
  )

  pacta_bonds_results_full <- pacta_bonds_results_full %>%
    dplyr::filter(!(scenario == "ETP2017_NPS" & ald_sector == "Power")) %>%
    # ADO 2473 - hotfix - ensure scenario names match across data sets (SPS is the follow up to NPS)
    dplyr::mutate(scenario = dplyr::if_else(stringr::str_detect(scenario, "WEO2019_SPS"), "WEO2019_NPS", scenario)) %>%
    dplyr::filter(scenario %in% scenarios) %>%
    dplyr::mutate(scenario = ifelse(stringr::str_detect(scenario, "_"), stringr::str_extract(scenario, "[^_]*$"), scenario)) %>%
    check_portfolio_consistency(start_year = start_year)

  pacta_bonds_results <- pacta_bonds_results_full %>%
    tidyr::complete(
      year = seq(start_year, start_year + time_horizon),
      nesting(!!!syms(nesting_vars))
    ) %>%
    dplyr::mutate(plan_tech_prod = dplyr::if_else(is.na(plan_tech_prod), 0, plan_tech_prod)) %>%
    apply_filters(
      investor = investor_name,
      sectors = sectors,
      technologies = technologies,
      scenario_geography_filter = scenario_geography_filter,
      scenarios = scenarios_filter,
      allocation_method = allocation_method_equity,
      start_analysis = start_year
    ) %>%
    dplyr::filter(
      allocation == allocation_method_equity,
      equity_market %in% equity_market_filter
    ) %>%
    dplyr::mutate(scenario = stringr::str_replace(scenario, "NPSRTS", "NPS")) %>%
    dplyr::distinct_all()

  if (nrow(pacta_bonds_results) <= 0) {
    print("Input pacta data has 0 valid rows after filtering. Skipping bonds calculation!")
  } else {

    # check scenario availability across data inputs for bonds
    check_scenario_availability(
      portfolio = pacta_bonds_results,
      scen_data = scenario_data,
      scenarios = scenarios_filter
    )

    bonds_port_aum <- sector_exposures %>%
      dplyr::group_by(investor_name, portfolio_name) %>%
      dplyr::filter(asset_type == "Bonds") %>%
      summarise(
        asset_portfolio_value = sum(valid_value_usd),
        .groups = "drop_last"
      )

    # Corporate bonds results (flat multiplier PRA 0.15) ---------------------------------------------------------

    bonds_results <- c()

    for (i in seq(1, nrow(transition_scenarios))) {
      transition_scenario_i <- transition_scenarios[i, ]

      # Calculate late and sudden prices for scenario i
      df_prices <- df_price %>%
        dplyr::mutate(Baseline = NPS) %>% # FIXME this should be parameterized!!
        dplyr::rename(
          year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
          SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
        ) %>%
        dplyr::group_by(ald_sector, technology) %>%
        dplyr::mutate(
          late_sudden_price = late_sudden_prices(
            SDS_price = SDS_price,
            Baseline_price = Baseline_price,
            year_of_shock = transition_scenario_i$year_of_shock,
            start_year = start_year,
            duration_of_shock = transition_scenario_i$duration_of_shock
          )
        ) %>%
        dplyr::ungroup()

      bonds_annual_profits <- pacta_bonds_results %>%
        convert_cap_to_generation(capacity_factors_power = capacity_factors_power) %>%
        extend_scenario_trajectory(
          scenario_data = scenario_data,
          start_analysis = start_year,
          end_analysis = end_year,
          time_frame = time_horizon
        ) %>%
        set_baseline_trajectory(
          scenario_to_follow_baseline = scenario_to_follow_baseline
        ) %>%
        set_ls_trajectory(
          scenario_to_follow_ls = scenario_to_follow_ls,
          shock_scenario = transition_scenario_i,
          scenario_to_follow_ls_aligned = scenario_to_follow_ls_aligned,
          start_year = start_year,
          end_year = end_year,
          analysis_time_frame = time_horizon
        )

      if (exists("excluded_companies")) {
        bonds_annual_profits <- bonds_annual_profits %>%
          exclude_companies(
            exclusion = excluded_companies,
            scenario_baseline = scenario_to_follow_baseline,
            scenario_ls = scenario_to_follow_ls
          )
      }

      bonds_annual_profits <- bonds_annual_profits %>%
        join_price_data(df_prices = df_prices) %>%
        join_net_profit_margins(net_profit_margins = net_profit_margins) %>%
        calculate_net_profits() %>%
        dcf_model_techlevel(discount_rate = discount_rate)

      plan_carsten_bonds <- pacta_bonds_results %>%
        dplyr::filter(
          year == start_year,
          technology %in% technologies,
          scenario_geography == scenario_geography_filter
        )

      if(identical(calculation_level, "company")) {
        plan_carsten_bonds <- plan_carsten_bonds %>%
          dplyr::distinct(investor_name, portfolio_name, company_name, ald_sector, technology,
                   scenario_geography, year, plan_carsten, plan_sec_carsten)

        if (!exists("excluded_companies")) {
          bonds_results <- dplyr::bind_rows(
            bonds_results,
            company_asset_value_at_risk(
              data = bonds_annual_profits,
              terminal_value = terminal_value,
              shock_scenario = transition_scenario_i,
              div_netprofit_prop_coef = div_netprofit_prop_coef,
              plan_carsten = plan_carsten_bonds,
              port_aum = bonds_port_aum,
              flat_multiplier = 0.15,
              exclusion = NULL
            )
          )
        } else {
          bonds_results <- dplyr::bind_rows(
            bonds_results,
            company_asset_value_at_risk(
              data = bonds_annual_profits,
              terminal_value = terminal_value,
              shock_scenario = transition_scenario_i,
              div_netprofit_prop_coef = div_netprofit_prop_coef,
              plan_carsten = plan_carsten_bonds,
              port_aum = bonds_port_aum,
              flat_multiplier = 0.15,
              exclusion = excluded_companies
            )
          )
        }

      } else {
        plan_carsten_bonds <- plan_carsten_bonds %>%
          dplyr::distinct(investor_name, portfolio_name, ald_sector, technology,
                   scenario_geography, year, plan_carsten, plan_sec_carsten)

        bonds_results <- dplyr::bind_rows(
          bonds_results,
          asset_value_at_risk(
            data = bonds_annual_profits,
            terminal_value = terminal_value,
            shock_scenario = transition_scenario_i,
            div_netprofit_prop_coef = div_netprofit_prop_coef,
            plan_carsten = plan_carsten_bonds,
            port_aum = bonds_port_aum,
            flat_multiplier = 0.15
          )
        )
      }

    }
  }
} else {
  print("No Bonds Portfolio Data available. Skipping!")
}


# Save output files if they exist  ------------------------------------------------------------------

# Output equity results
if (exists("equity_results")) {
  equity_results %>% write_results(
    path_to_results = results_path,
    investorname = pf_name,
    asset_type = "equity",
    level = calculation_level,
    file_type = "rda"
  )
}
# Output bonds results
if (exists("bonds_results")) {
  bonds_results %>% write_results(
    path_to_results = results_path,
    investorname = pf_name,
    asset_type = "bonds",
    level = calculation_level,
    file_type = "rda"
  )
}
