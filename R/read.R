st_read_agnostic <- function(dir, start_year, sectors, risk_type) {

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
      financial_data = read_financial_data(financial_data_file(dir)),
      production_data = read_production_data(production_data_file(dir)))

     if (risk_type == "trisk") {
      carbon_data = read_carbon_data(carbon_price_data_file(dir))

      out[[length(out)+1]] <- carbon_data
      }
browser()

  # } else {
  #   out <- list(
  #     capacity_factors_power = capacity_factors_power,
  #     df_price = read_price_data(price_data_file(dir)),
  #     scenario_data = read_scenario_data(scenario_data_file(dir, start_year)),
  #     financial_data = read_financial_data(financial_data_file(dir)),
  #     production_data = read_production_data(production_data_file(dir))
  #   )
  # }

  return(out)
}
