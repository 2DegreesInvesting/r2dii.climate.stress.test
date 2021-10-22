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
