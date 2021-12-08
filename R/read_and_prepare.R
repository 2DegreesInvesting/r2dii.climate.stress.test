#' Read and prepare project agnostic data
#'
#' Function reads in data that are independent of the project and conducts some
#' checking and wrangling.
#'
#' @param start_year Start year of analysis.
#' @param end_year End year of analysis.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded.
#' @param scenario_geography_filter String holding geographic asset locations to
#'   be considered in analysis.
#' @param asset_type Type of asset, can be bonds, loans or equity.
#' @param path String holding path to data.
#'
#' @return A list holding prepared project agnostic data.
read_and_prepare_project_agnostic_data <- function(start_year, end_year, company_exclusion,
                                                   scenario_geography_filter, asset_type,
                                                   path) {
  if (!asset_type %in% asset_types_lookup) {
    stop("Invalid asset type provided.")
  }

  capacity_factors_power <- read_capacity_factors(
    path = file.path(path, "capacity_factors_WEO_2020.csv")
  )

  if (company_exclusion) {
    excluded_companies <- validate_file_exists(file.path(path, "exclude-companies.csv")) %>%
      readr::read_csv(
        col_types = readr::cols(
          company_name = "c",
          technology = "c"
        )
      )
  } else {
    excluded_companies <- NULL
  }

  df_price <- read_price_data_old(
    path = file.path(path, paste0("prices_data_", price_data_version_lookup, ".csv")),
    expected_technologies = technologies_lookup
  ) %>%
    dplyr::filter(year >= start_year) %>%
    check_price_consistency(start_year = start_year)

  scenario_data <- read_scenario_data(
    path = file.path(path, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
  ) %>%
    wrangle_scenario_data(start_year = start_year, end_year = end_year) %>%
    dplyr::filter(
      .data$ald_sector %in% sectors_lookup &
        .data$technology %in% technologies_lookup &
        .data$scenario_geography == scenario_geography_filter
    )

  financial_data <- read_financial_data(
    path = file.path(path, "prewrangled_financial_data_stress_test.csv")
  ) %>%
    check_financial_data(asset_type = asset_type)

  return(list(
    capacity_factors_power = capacity_factors_power,
    excluded_companies = excluded_companies,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data
  ))
}

#' Read and prepare project specific data
#'
#' Function reads in data that are specific the project and conducts some
#' checking and wrangling. Also infers start_year of analysis.
#'
#' @inheritParams validate_input_values
#' @inheritParams read_and_prepare_project_agnostic_data
#' @inheritParams wrangle_and_check_pacta_results
#' @param calculation_level String holding level of calculation.
#' @param path String holding path to data.
#'
#' @return A list of lists holding prepared project specific data.
read_and_prepare_project_specific_data <- function(asset_type, calculation_level,
                                                   time_horizon, scenario_geography_filter,
                                                   scenarios_filter, equity_market_filter,
                                                   term, path) {
  pacta_results <- read_pacta_results(
    path = file.path(path, paste0(stringr::str_to_title(asset_type), "_results_", calculation_level, ".rda")),
    level = calculation_level
  )

  start_year <- min(pacta_results$year, na.rm = TRUE)

  wrangled_pacta_results <- pacta_results %>%
    wrangle_and_check_pacta_results(
      start_year = start_year,
      time_horizon = time_horizon,
      scenario_geography_filter = scenario_geography_filter,
      scenarios_filter = scenarios_filter,
      equity_market_filter = equity_market_filter
    ) %>%
    # ADO 1943 - for the time being, one global term value is set by the user.
    # TODO: ADO 3182 - allow setting loan level term
    dplyr::mutate(term = term)

  sector_exposures <- read_sector_exposures(file.path(path, "overview_portfolio.rda")) %>%
    wrangle_and_check_sector_exposures(asset_type = asset_type)
  # TODO: potentially convert currencies to USD or at least common currency

  return(
    list(
      data_list = list(
        pacta_results = wrangled_pacta_results,
        sector_exposures = sector_exposures
      ),
      start_year = start_year
    )
  )
}


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
