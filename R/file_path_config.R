capacity_factor_file <- function(dir) {
  fp <- file.path(dir, "prewrangled_capacity_factors.csv")
  out <- ifelse(file.exists(fp), fp, NULL)
  return(out)
}

price_data_file <- function(dir) {
  fp <- file.path(dir, "price_data_long.csv")
  out <- ifelse(file.exists(fp), fp, NULL)
  return(out)
}

scenario_data_file <- function(dir) {
  fp <- file.path(dir, glue::glue("Scenarios_AnalysisInput.csv"))
  out <- ifelse(file.exists(fp), fp, NULL)
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
  fp <- file.path(dir, "ngfs_carbon_price.csv")
  out <- ifelse(file.exists(fp), fp, NULL)
  return(out)
}
