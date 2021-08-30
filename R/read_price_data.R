#' Read in price data from csv and check that all expected columns are given.
#'
#' @param path A string that points to the location of the file containing the
#'   price data
#' @param version A string indicating whether to prepare the capacity factors
#'   based on the old price data structure or the new long format. Must be
#'   either "old" or "new".
#' @param expected_technologies A character vector listing all technologies for
#'   which price data must be provided
#'
#' @family import functions
#'
#' @export
read_price_data <- function(path,
                            version,
                            expected_technologies) {
  validate_file_exists(path)

  version_allowed <- version %in% c("old", "new")
  stopifnot(version_allowed)

  if (version == "new") {
    data <- read_price_data_internal(path = path)
  } else {
    data <- read_price_data_internal_old(path = path)
  }

  prices_for_all_technologies_available <- all(
    expected_technologies %in% unique(data$technology)
  )
  stopifnot(prices_for_all_technologies_available)

  return(data)
}


#' Read in price data from csv and check that all expected columns are given.
#'
#' @description This function reads in price data using long file format.
#'   It is expected to work with data based on IEA WEO 2020.
#' @inheritParams read_price_data
#'
#' @export
read_price_data_internal <- function(path) {
  validate_file_exists(path)

  data <- readr::read_csv(
    path,
    col_types = readr::cols(
      year = "d",
      source = "c",
      scenario = "c",
      scenario_geography = "c",
      technology = "c",
      indicator = "c",
      unit = "c",
      price = "d"
    )
  )

  data %>% validate_data_has_expected_cols(
    expected_columns = c(
      "year", "source", "scenario", "scenario_geography", "technology",
      "indicator", "unit", "price"
    )
  )

  return(data)
}



#' Read in price data from csv and check that all expected columns are given.
#'
#' @description This function reads in price data using the old wide data format.
#' @inheritParams read_price_data
#'
#' @export
read_price_data_internal_old <- function(path) {
  validate_file_exists(path)

  data <- readr::read_csv(
    path,
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
  )

  data %>% validate_data_has_expected_cols(
    expected_columns = c(
      "year", "sector", "technology", "sector_unit_ds", "price_unit_iea",
      "price_unit_etr", "B2DS", "b2ds_source", "NPS", "nps_source", "SDS",
      "sds_source", "Baseline", "baseline_source"
    )
  )

  return(data)
}
