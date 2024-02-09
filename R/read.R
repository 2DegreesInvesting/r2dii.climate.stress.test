st_read_agnostic <- function(dir, risk_type) {
  # capacity_factors are only needed for power sector
  capacity_factors_power <- read_capacity_factors_power(capacity_factor_file(dir))

  out <- list(
    capacity_factors_power = capacity_factors_power,
    df_price = read_price_data(price_data_file(dir)),
    scenario_data = read_scenario_data(scenario_data_file(dir)),
    financial_data = read_financial_data(financial_data_file(dir)),
    production_data = read_production_data(production_data_file(dir))
  )

  ## read in dataframe with carbon prices in the trisk model lrisk does not have carbon tax option
  if (risk_type == "trisk") {
    carbon_data <- read_carbon_data(carbon_price_data_file(dir))

    out$carbon_data <- carbon_data
  }

  return(out)
}
