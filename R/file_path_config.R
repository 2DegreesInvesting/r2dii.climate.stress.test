financial_data_file <- function(dir) {
  out <- file.path(dir, "prewrangled_financial_data_stress_test.csv")
  return(out)
}

production_data_file <- function(dir) {
  out <- file.path(dir, "abcd_stress_test_input.csv")
  return(out)
}
