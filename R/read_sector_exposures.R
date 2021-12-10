#' Read sector exposures
#'
#' Read file holding sector exposure results.
#'
#' @param path Path to file holding sector exposures.
#'
#' @return A tibble holding sector exposure data
read_sector_exposures <- function(path) {
  sector_exposures <- validate_file_exists(path) %>%
    readr::read_rds()

  validate_data_has_expected_cols(
    data = sector_exposures,
    expected_columns = c(
      "investor_name", "portfolio_name", "valid_value_usd", "valid_input",
      "asset_type", "financial_sector"
    )
  )

  return(sector_exposures)
}
