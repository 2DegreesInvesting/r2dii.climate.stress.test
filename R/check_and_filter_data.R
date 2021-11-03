#' Check and filter data
#'
#' Function filters dataset so that only rows relevant for analysis remain. Also
#' an error is thrown if duplicate entries are detected.
#'
#' Note that dataset `sector_exposures` is not filtered for considered sectors
#' as data is used in [calculate_aum()] where the asset under management of the
#' entire portfolio is calculated.
#'
#' @param st_data_list A list holding imported and prewrangled stress test input
#'   data.
#' @param start_year Numeric holding start year of analysis.
#' @param end_year Numeric holding end year of analysis.
#' @param scenarios_filter String vector holding name of baseline and shock
#'   scenario.
#' @param scenario_geography_filter String holding name of considered
#'   geographical regions.
#'
#' @return List `st_data_list` with tibbles subset to rows required for
#'   analysis.
check_and_filter_data <- function(st_data_list, start_year, end_year,
                                  scenarios_filter, scenario_geography_filter) {
  capacity_factors_power_filtered <- st_data_list$capacity_factors_power %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year))

  excluded_companies_filtered <- st_data_list$excluded_companies %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup)

  df_price_filtered <- st_data_list$df_price %>%
    dplyr::filter(.data$sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year))

  scenario_data_filtered <- st_data_list$scenario_data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year))

  financial_data_filtered <- st_data_list$financial_data %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup)

  pacta_results_filtered <- st_data_list$pacta_results %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$ald_sector %in% .env$sectors_lookup) %>%
    dplyr::filter(.data$technology %in% .env$technologies_lookup) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year))

  data_list <- list(
    capacity_factors_power = capacity_factors_power_filtered,
    excluded_companies = excluded_companies_filtered,
    df_price = df_price_filtered,
    scenario_data = scenario_data_filtered,
    financial_data = financial_data_filtered,
    pacta_results = pacta_results_filtered,
    sector_exposures = st_data_list$sector_exposures
  )

  cuc_list <- list(
    c("scenario", "scenario_geography", "technology", "year"),
    c("company_name", "technology"),
    c("year", "sector", "technology"),
    c("scenario_geography", "scenario", "ald_sector", "technology", "year"),
    c("company_name", "company_id", "ald_sector", "technology"),
    c("year", "equity_market", "ald_sector", "technology", "scenario", "allocation",
      "scenario_geography", "company_name", "id", "investor_name", "portfolio_name"),
    c("financial_sector", "investor_name", "portfolio_name")
  )

  name_list <- list(
    "capacity_factors_power", "excluded_companies", "df_price", "scenario_data",
    "financial_data","pacta_results", "sector_exposures"
    )

  mapply(function(data, cuc_cols) {
    report_all_duplicate_kinds(data = data, composite_unique_cols = cuc_cols)
  },
  data_list, cuc_list)

  mapply(function(data, name_list) {
    report_missings(data = data, name_data = name_list)
  },
  data_list, name_list)

  has_negative_values <- pacta_results_filtered %>%
    dplyr::select(plan_tech_prod, plan_carsten, scen_tech_prod, plan_sec_prod, plan_sec_carsten) %>%
    purrr::map_lgl(function(col) {
      any(col < 0)
    })

  if (any(has_negative_values)) {
    stop(
      paste0("Detected negative values on columns ",
             paste0(names(has_negative_values[has_negative_values == TRUE]), collapse = ", "),
             ". Analysis cannot process this."),
      call. = FALSE
    )
  }

  return(data_list)
}
