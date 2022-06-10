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
    dplyr::select(.data$year, .data$scenario, ald_sector = .data$sector, .data$technology, .data$price)

  return(data)
}

#' Read in price data
#'
#' This function reads in price data using the old wide data format.
#'
#' @inheritParams read_price_data
#' @param expected_technologies String vector holding expected technologies.
#'
#' @return A tibble holding price data in long format.
read_price_data_old <- function(path, expected_technologies) {
  data <- validate_file_exists(path) %>%
    readr::read_csv(
      col_types = readr::cols(
        year = "d",
        sector = "c",
        technology = "c",
        sector_unit_ds = "c",
        price_unit_iea = "c",
        price_unit_etr = "c",
        B2DS = "d",
        b2ds_source = "c",
        NPS = "d",
        nps_source = "c",
        SDS = "d",
        sds_source = "c",
        Baseline = "d",
        baseline_source = "c"
      )
    ) %>%
    check_technology_availability(expected_technologies = expected_technologies)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "sector", "technology", "sector_unit_ds", "price_unit_iea",
      "price_unit_etr", "B2DS", "b2ds_source", "NPS", "nps_source", "SDS",
      "sds_source", "Baseline", "baseline_source"
    )
  )

  return(data)
}
