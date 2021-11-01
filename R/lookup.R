# An investor name is needed for legacy reasons currently there is no
# practical purpose.
investor_name_placeholder <- "Meta Investor"

#' Help lookup values
#'
#' @name lookup
#'
#' @examples
#' nesting_vars_lookup
#'
#' asset_types_lookup
#'
#' scenarios_lookup
#'
#' baseline_scenario_lookup
#'
#' shock_scenario_lookup
#'
#' calculation_level_lookup
#'
#' lgd_senior_claims_range_lookup
#'
#' lgd_subordinated_claims_range_lookup
#'
#' terminal_value_range_lookup
#'
#' risk_free_rate_range_lookup
#'
#' discount_rate_range_lookup
#'
#' div_netprofit_prop_coef_range_lookup
#'
#' shock_year_range_lookup
#'
#' term_range_lookup
#'
#' end_year_lookup
#'
#' credit_type_lookup
#'
#' sectors_lookup
#'
#' technologies_lookup
#'
#' allocation_method_lookup
#'
#' p4i_p4b_sector_technology_lookup
#'
#' p4i_p4b_scenario_lookup
#'
#' p4b_scenarios_lookup
#'
#' p4b_production_data_years_lookup
#'
#' p4b_scenario_data_years_lookup
NULL

# vector holding vars to nest pacta results by
#' @rdname lookup
#' @export
nesting_vars_lookup <- c(
  "investor_name", "portfolio_name", "equity_market", "ald_sector", "technology",
  "scenario", "allocation", "scenario_geography", "company_name"
)

#' @rdname lookup
#' @export
asset_types_lookup <- c("equity", "bonds", "loans")

# vector holding scenarios to filter PACTA results by for stresstesting
#' @rdname lookup
#' @export
scenarios_lookup <- c(
  "ETP2017_NPS",
  "ETP2017_SDS",
  "WEO2019_NPS",
  "WEO2019_SDS"
)

#' @rdname lookup
#' @export
baseline_scenario_lookup <- "NPS"
#' @rdname lookup
#' @export
shock_scenario_lookup <- "SDS"
#' @rdname lookup
#' @export
calculation_level_lookup <- "company"

#' @rdname lookup
#' @export
lgd_senior_claims_range_lookup <- c(0.3, 0.6)
#' @rdname lookup
#' @export
lgd_subordinated_claims_range_lookup <- c(0.6, 0.9)
#' @rdname lookup
#' @export
terminal_value_range_lookup <- c(0, 0.1)
#' @rdname lookup
#' @export
risk_free_rate_range_lookup <- c(0, 0.05)
#' @rdname lookup
#' @export
discount_rate_range_lookup <- c(-0.01, 0.05)
#' @rdname lookup
#' @export
div_netprofit_prop_coef_range_lookup <- c(0.8, 1.0)
#' @rdname lookup
#' @export
shock_year_range_lookup <- c(2025, 2040)
#' @rdname lookup
#' @export
term_range_lookup <- c(1, 10)

#' @rdname lookup
#' @export
end_year_lookup <- 2040

#' @rdname lookup
#' @export
credit_type_lookup <- c("outstanding", "credit_limit")

# vector holding considered sectors in stress testing
#' @rdname lookup
#' @export
sectors_lookup <- c("Power", "Oil&Gas", "Coal", "Automotive")

# vector holding considered technologies in stress testing
#' @rdname lookup
#' @export
technologies_lookup <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)

# holding allocation method used for stress testing
#' @rdname lookup
#' @export
allocation_method_lookup <- "portfolio_weight"

# technology and sector mapping between P4I and P4B
# HDV and shipping not consistently defined across both versions at this time
#' @rdname lookup
#' @export
p4i_p4b_sector_technology_lookup <- tibble::tribble(
  ~sector_p4b,   ~technology_p4b,             ~sector_p4i,    ~technology_p4i,
  "automotive",  "electric",                 "Automotive",   "Electric",
  "automotive",  "hybrid",                   "Automotive",   "Hybrid",
  "automotive",  "fuelcell",                 "Automotive",   "FuelCell",
  "automotive",  "ice",                      "Automotive",   "ICE",
  "coal",        "coal",                     "Coal",         "Coal",
  "oil and gas", "gas",                      "Oil&Gas",      "Gas",
  "oil and gas", "oil",                      "Oil&Gas",      "Oil",
  "power",       "coalcap",                  "Power",        "CoalCap",
  "power",       "gascap",                   "Power",        "GasCap",
  "power",       "hydrocap",                 "Power",        "HydroCap",
  "power",       "nuclearcap",               "Power",        "NuclearCap",
  "power",       "oilcap",                   "Power",        "OilCap",
  "power",       "renewablescap",            "Power",        "RenewablesCap",
  "aviation",    "freight",                  "Aviation",     "Freight",
  "aviation",    "passenger",                "Aviation",     "Passenger",
  "cement",      "grinding",                 "Cement",       "Grinding",
  "cement",      "integrated facility",      "Cement",       "Integrated facility",
  "steel",       "ac-electric arc furnace",  "Steel",        "Ac-Electric Arc Furnace",
  "steel",       "bof shop",                 "Steel",        "Bof Shop",
  "steel",       "dc-electric arc furnace",  "Steel",        "Dc-Electric Arc Furnace",
  "steel",       "open hearth meltshop",     "Steel",        "Open Hearth Meltshop"
)

# scenario mapping between P4I and P4B
# TODO: should the implicit mappin a la sps is the follow up NPS be hapening elsewhere?
#' @rdname lookup
#' @export
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

# P4B scenario list
#' @rdname lookup
#' @export
p4b_scenarios_lookup <- c("target_b2ds", "target_cps", "target_rts",
                          "target_sps", "target_steps", "target_2ds",
                          "target_sds")

# allowed input years p4b production data file
#' @rdname lookup
#' @export
p4b_production_data_years_lookup <- c(2020, 2021)

# allowed input years p4b scenario data file
#' @rdname lookup
#' @export
p4b_scenario_data_years_lookup <- c(2020, 2021)
