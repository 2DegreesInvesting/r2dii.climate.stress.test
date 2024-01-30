#' Read in price data
#'
#' This function reads in price data using long file format. It is expected to
#' work with data based on IEA WEO 2020.
#' 
#' @return A tibble holding price data in long format.
read_price_data <- function() {
  data <- STDataMGMT::price_data_long

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "scenario", "scenario_geography", "ald_business_unit",
      "indicator", "unit", "price"
    )
  )

  data <- data %>%
    dplyr::select_at(c("year", "scenario", "ald_sector", "ald_business_unit", "price"))

  return(data)
}
