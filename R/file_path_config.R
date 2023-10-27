capacity_factor_file <- function(dir) {
  out <- file.path(dir, "prewrangled_capacity_factors.parquet")
  return(out)
}

price_data_file <- function(dir) {
  out <- file.path(dir, "price_data_long.parquet")
  return(out)
}

scenario_data_file <- function(dir, start_year) {
  out <- file.path(dir, glue::glue("Scenarios_AnalysisInput_{start_year}.parquet"))
  return(out)
}

financial_data_file <- function(dir) {
  out <- file.path(dir, "prewrangled_financial_data_stress_test.parquet")
  return(out)
}

production_data_file <- function(dir) {
  out <- file.path(dir, "abcd_stress_test_input.parquet")
  return(out)
}

carbon_price_data_file <- function(dir) {
  out <- file.path(dir, "ngfs_carbon_price.parquet")
  return(out)
}
