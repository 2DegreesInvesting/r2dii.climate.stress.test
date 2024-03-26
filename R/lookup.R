#' Lookup valid values
#'
#' @name lookup
#'
#' @examples
#'
#' credit_type_lookup
NULL

MAX_POSSIBLE_YEAR <- 2050

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

# styler: on

# holds names of input arguments to run_*risk that are not model parameters
setup_vars_lookup <- c("input_path", "output_path", "iter_var", "return_results")

high_carbon_tech_lookup <- c("ICE", "Coal", "Oil", "Gas", "CoalCap", "GasCap", "OilCap")

countries_for_regions_mapper_lookup <- tibble::tibble(
  region = c("brazil", "brazil", "india", "india", "japan", "japan", "russia", "russia", "south africa", "south africa", "united states", "united states"),
  isos = c("br", "br", "in", "in", "jp", "jp", "ru", "ru", "za", "za", "us", "us"),
  source = rep(c("weo_2019", "weo_2020"), 6)
)

cuc_capacity_factors_power <- c("scenario", "scenario_geography", "ald_business_unit", "year")
cuc_price_data <- c("year", "ald_sector", "ald_business_unit", "scenario")
cuc_financial_data <- c("company_id")
cuc_scenario_data <- c("scenario_geography", "scenario", "ald_sector", "ald_business_unit", "year")


cuc_production_data <- c(
  "company_name", "company_id", "year", "ald_sector", "ald_business_unit", "scenario_geography"
)
