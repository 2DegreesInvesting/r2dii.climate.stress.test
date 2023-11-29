#' Lookup valid values
#'
#' @name lookup
#'
#' @examples
#'
#' credit_type_lookup
NULL

# An investor name is needed for legacy reasons currently there is no
# practical purpose.
investor_name_placeholder <- "Meta Investor"

calculation_level_lookup <- "company"

# TODO this value must be removed everywhere it is referred
# and the code using it must be refactored to not use it anymore.
# It seems it refers to the time horizon in the production data
# It is confusing to have this variable as well as end_year and start_year
time_horizon_lookup <- 5

flat_multiplier_lookup <- 1

#' @rdname lookup
#' @export
credit_type_lookup <- c("outstanding", "credit_limit")

# holding allocation method used for stress testing
allocation_method_lookup <- "portfolio_weight"

equity_market_filter_lookup <- "GlobalMarket"

# technology and sector mapping between P4I and P4B
# HDV and shipping not consistently defined across both versions at this time
# styler: off
p4i_p4b_sector_technology_lookup <- tibble::tribble(
  ~sector_p4b,   ~technology_p4b,             ~sector_p4i,    ~technology_p4i,
  "automotive",  "electric",                 "Automotive",   "Electric",
  "automotive",  "fuelcell",                 "Automotive",   "FuelCell",
  "automotive",  "hybrid",                   "Automotive",   "Hybrid",
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

# styler: on

# holds names of input arguments to run_*risk that are not model parameters
setup_vars_lookup <- c("input_path", "output_path", "iter_var", "return_results")

high_carbon_tech_lookup <- c("ICE", "Coal", "Oil", "Gas", "CoalCap", "GasCap", "OilCap")

countries_for_regions_mapper_lookup <- tibble::tibble(
  region = c("brazil", "brazil", "india", "india", "japan", "japan", "russia", "russia", "south africa", "south africa", "united states", "united states"),
  isos = c("br", "br", "in", "in", "jp", "jp", "ru", "ru", "za", "za", "us", "us"),
  source = rep(c("weo_2019", "weo_2020"), 6)
)

cuc_capacity_factors_power <- c("scenario", "scenario_geography", "technology", "year")
cuc_price_data <- c("year", "ald_sector", "technology", "scenario")
cuc_financial_data <- c("company_id")
cuc_scenario_data <- c("scenario_geography", "scenario", "ald_sector", "technology", "year")

cuc_production_data <- c(
  "company_name", "id", "year", "ald_sector", "technology", "scenario_geography"
)
