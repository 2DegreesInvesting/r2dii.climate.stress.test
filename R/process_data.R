process_pacta_results <- function(data) {

}

process_sector_exposures <- function(data) {

}

process_capacity_factors_power <- function(data,
                                           scenarios_filter,
                                           scenario_geography_filter,
                                           technologies,
                                           start_year,
                                           end_year) {
  data_processed <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios_filter) %>%
    dplyr::filter(.data$scenario_geography %in% .env$scenario_geography_filter) %>%
    dplyr::filter(.data$technology %in% .env$technologies) %>%
    dplyr::filter(dplyr::between(.data$year, .env$start_year, .env$end_year)) %>%
    report_all_duplicate_kinds(data = data, composite_unique_cols = cuc_capacity_factors_power) %>%
    report_missings(name_data = "capacity factors", throw_error = TRUE)

  return(data_processed)
}

process_excluded_companies <- function(data) {

}

process_df_price <- function(data) {

}

process_scenario_data <- function(data) {

}

process_financial_data <- function(data) {

}