#' Display the annual pd changes per company and technology.
#'
#' @param data A data.frame that contains Merton output data.
#' @param shock_year Numeric. Vector of length 1 that specifies the year of the
#'   policy shock.
#' @param company_filter Character. A vector of companies to display.
#' @param geography_filter Character. A vector containing the geographical
#'   specification of the scenario
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
annual_pd_change_company_technology <- function(data,
                                                shock_year = NULL,
                                                company_filter = NULL,
                                                geography_filter = NULL) {
  force(data)

  shock_year %||% stop("Must provide input for 'shock_year'", call. = FALSE)
  company_filter %||% stop("Must provide input for 'company_filter'", call. = FALSE)
  geography_filter %||% stop("Must provide input for 'geography_filter'", call. = FALSE)

  data_has_expected_columns <- all(c(
    "scenario_name", "company_name", "ald_sector", "technology",
    "scenario_geography", "year", "PD_change"
  )
  %in% colnames(data))
  stopifnot(data_has_expected_columns)

  shock_year <- stringr::str_c("Carbon balance ", shock_year)

  plot <- data %>%
    dplyr::filter(
      .data$company_name %in% .env$company_filter,
      .data$scenario_name == .env$shock_year,
      .data$scenario_geography %in% geography_filter
    ) %>%
    ggplot(
      aes(
        x = .data$year,
        y = .data$PD_change * 100,
        fill = .data$PD_change * 100
      )
    ) +
    geom_col(position = "dodge") +
    labs(
      # TODO: must display full years
      x = "Year",
      y = "PD change in % points",
      fill = "PD change in % points",
      title = "Annual PD change by company, technology",
      subtitle = shock_year
    ) +
    scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red") +
    facet_grid(
      rows = vars(.data$ald_sector, .data$technology),
      cols = vars(.data$company_name),
      scales = "free"
    ) +
    r2dii.plot::theme_2dii() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(2, 2, 14, 2)
      )
    )
}
