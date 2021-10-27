# An investor name is needed for legacy reasons currently there is no
# practical purpose.
investor_name_placeholder <- "Meta Investor"

# vector holding vars to nest pacta results by
nesting_vars_lookup <- c(
  "investor_name", "portfolio_name", "equity_market", "ald_sector", "technology",
  "scenario", "allocation", "scenario_geography", "company_name"
)

asset_types_lookup <- c("equity", "bonds", "loans")

# vector holding scenarios to filter PACTA results by for stresstesting
scenarios_lookup <- c(
  "ETP2017_NPS",
  "ETP2017_SDS",
  "WEO2019_NPS",
  "WEO2019_SDS"
)

baseline_scenario_lookup <- "NPS"
shock_scenario_lookup <- "SDS"
calculation_level_lookup <- "company"

lgd_senior_claims_range_lookup <- c(0.3, 0.6)
lgd_subordinated_claims_range_lookup <- c(0.6, 0.9)
terminal_value_range_lookup <- c(0, 0.1)
risk_free_rate_range_lookup <- c(0, 0.05)
discount_rate_range_lookup <- c(-0.01, 0.05)
div_netprofit_prop_coef_range_lookup <- c(0.8, 1.0)
shock_year_range_lookup <- c(2025, 2040)
term_range_lookup <- c(1, 10)

end_year_lookup <- 2040

credit_type_lookup <- c("outstanding", "credit_limit")

# vector holding considered sectors in stress testing
sectors_lookup <- c("Power", "Oil&Gas", "Coal", "Automotive")

# vector holding considered technologies in stress testing
technologies_lookup <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)

# holding allocation method used for stress testing
allocation_method_lookup <- "portfolio_weight"

# sector mapping between P4I and P4B
p4i_p4b_sectors_lookup <- tibble::tribble(
  ~sector_p4b,   ~sector_p4i,
  "power",       "Power",
  "oil and gas", "Oil&Gas",
  "coal",        "Coal",
  "automotive",  "Automotive",
  "steel",       "Steel",
  "cement",      "Cement"
)

# technology mapping between P4I and P4B
p4i_p4b_technology_lookup <- tibble::tribble(
  ~technology_p4b,  ~technology_p4i,
  "coalcap",        "CoalCap",
  "gascap",         "GasCap",
  "renewablescap",  "RenewablesCap",
  "nuclearcap",     "NuclearCap",
  "hydrocap",       "HydroCap",
  "oilcap",         "OilCap",
  "oil",            "Oil",
  "gas",            "Gas",
  "coal",           "Coal",
  "electric",       "Electric",
  "hybrid",         "Hybrid",
  "ice",            "ICE",
)

# scenario mapping between P4I and P4B
# TODO: should the implicit mappin a la sps is the follow up NPS be hapening elsewhere?
p4i_p4b_scenario_lookup <- tibble::tribble(
  ~scenario_p4b,   ~scenario_p4i,
  "target_cps",   "CPS",
  "target_rts",   "NPS",
  "target_sps",   "NPS",
  "target_steps", "NPS",
  "target_2ds",   "SDS",
  "target_sds",   "SDS",
  "target_b2ds",  "B2DS",
)
