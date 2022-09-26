#' Create key for removal of invalid entries
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio).
#'
#' @return data frame with one additional variable, `key`
#' @noRd
add_key_for_invalid_entries <- function(data) {
  data <- data %>%
    dplyr::mutate(
      key = paste(
        .data$company_name, .data$id, .data$ald_sector, .data$technology,
        .data$scenario_geography
      )
    )
  data
}
