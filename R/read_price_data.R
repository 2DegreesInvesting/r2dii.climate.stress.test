#' Read in price data
#'
#' This function reads in price data using long file format. It is expected to
#' work with data based on IEA WEO 2020.
#'
#' @param path A string that points to the location of the file containing the
#'   price data
#' @return A tibble holding price data in long format.
read_price_data <- function(path) {
  data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        year = "d",
        scenario = "c",
        scenario_geography = "c",
        technology = "c",
        indicator = "c",
        unit = "c",
        price = "d"
      )
    )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "scenario", "scenario_geography", "technology",
      "indicator", "unit", "price"
    )
  )

  data <- data %>%
    # doing hardcoded filtering directly upon import as we currently do not
    # differentiate scenario_geographies for price data
    dplyr::filter(scenario_geography == "Global") %>%
    dplyr::select(all_of(c("year", "scenario", "ald_sector" = "sector", "technology", "price")))

  return(data)
}
