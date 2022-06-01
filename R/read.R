st_read_specific <- function(dir, asset_type, use_company_terms) {
  out <- list(
    pacta_results = read_pacta_results(pacta_results_file(dir, asset_type)),
    sector_exposures = read_sector_exposures(sector_exposures_file(dir)),
    company_terms = read_company_terms(company_terms_file(dir), use_company_terms)
  )

  return(out)
}

st_read_agnostic <- function(dir, start_year, sectors) {

  # capacity_factors are only needed for power sector
  if ("Power" %in% sectors) {
    capacity_factors_power <- read_capacity_factors_power(capacity_factor_file(dir))
  } else {
    capacity_factors_power <- NULL
  }

  out <- list(
    capacity_factors_power = capacity_factors_power,
    df_price = read_price_data(price_data_file(dir)),
    scenario_data = read_scenario_data(scenario_data_file(dir, start_year)),
    financial_data = read_financial_data(financial_data_file(dir))
  )

  return(out)
}
