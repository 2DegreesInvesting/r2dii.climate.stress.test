capacity_factor_file <- function(dir) {
  out <- file.path(dir, "prewrangled_capacity_factors.csv")
  return(out)
}

price_data_file <- function(dir) {
  out <- file.path(dir, "price_data_long.csv")
  return(out)
}

scenario_data_file <- function(dir, start_year_two) {
  start_year_two <- 2021
  out <- file.path(dir, glue::glue("Scenarios_AnalysisInput_{start_year_two}.csv"))
  return(out)
}

financial_data_file <- function(dir) {
  out <- file.path(dir, "prewrangled_financial_data_stress_test.csv")
  return(out)
}

production_data_file <- function(dir) {
  out <- file.path(dir, "abcd_stress_test_input.csv")
  return(out)
}

carbon_price_data_file <- function(dir) {
  out <- file.path(dir, "ngfs_carbon_price.csv")
  return(out)
}
