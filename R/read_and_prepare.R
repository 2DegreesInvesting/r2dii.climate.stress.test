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
#'
#' @return A list holding prepared project agnostic data.
read_and_prepare_project_agnostic_data <- function(start_year, end_year, company_exclusion,
                                                   scenario_geography_filter, asset_type) {

  if (!asset_type %in% asset_types_lookup) {
    stop("Invalid asset type provided.")
  }

  data_location <- get_st_data_path()

  capacity_factors_power <- read_capacity_factors(
    path = file.path(data_location, "capacity_factors_WEO_2020.csv"),
    version = "new"
  )

  if (company_exclusion) {
    excluded_companies <- readr::read_csv(
      file.path(data_location, "exclude-companies.csv"),
      col_types = readr::cols(
        company_name = "c",
        technology = "c"
      )
    )
  } else {
    excluded_companies <- NULL
  }

  df_price <- read_price_data(
    path = file.path(data_location, paste0("prices_data_", config::get(file = "st_project_settings.yml")$price_data_version, ".csv")),
    version = "old",
    expected_technologies = technologies_lookup
  ) %>%
    dplyr::filter(year >= start_year) %>%
    check_price_consistency(start_year = start_year)

  scenario_data <- read_scenario_data(
    path = file.path(data_location, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
  ) %>%
    wrangle_scenario_data(start_year = start_year, end_year = end_year) %>%
    dplyr::filter(
      .data$ald_sector %in% sectors_lookup &
        .data$technology %in% technologies_lookup &
        .data$scenario_geography == scenario_geography_filter
    )

  financial_data <- read_company_data(
    path = get(asset_type, create_stressdata_masterdata_file_paths()),
    asset_type = asset_type
  ) %>%
    wrangle_financial_data(start_year = start_year)

  return(list(
    capacity_factors_power = capacity_factors_power,
    excluded_companies = excluded_companies,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data
  ))
}
