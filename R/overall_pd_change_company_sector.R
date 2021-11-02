#' Display the overall pd changes per maturity, company and technology.
#'
#' @param data A data.frame that contains Merton output data.
#' @param shock_year Numeric. Vector of length 1 that specifies the year of the
#'   policy shock.
#' @param sector_filter Character. A vector of sectors to display.
#' @param company_filter Character. A vector of companies to display.
#' @param geography_filter Character. A vector containing the geographical
#'   specification of the scenario
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
overall_pd_change_company_sector <- function(data,
                                             shock_year = NULL,
                                             sector_filter = NULL,
                                             company_filter = NULL,
                                             geography_filter = NULL) {
  force(data)

  shock_year %||% stop("Must provide input for 'shock_year'", call. = FALSE)
  sector_filter %||% stop("Must provide input for 'sector_filter'", call. = FALSE)
  company_filter %||% stop("Must provide input for 'company_filter'", call. = FALSE)
  geography_filter %||% stop("Must provide input for 'geography_filter'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "scenario_name", "company_name", "ald_sector", "scenario_geography", "term",
      "PD_change"
    )
  )

  shock_year <- stringr::str_c("Carbon balance ", shock_year)

  plot <- data %>%
    dplyr::filter(
      ald_sector %in% .env$sector_filter,
      company_name %in% .env$company_filter,
      scenario_name == .env$shock_year,
      .data$scenario_geography %in% geography_filter
    ) %>%
    ggplot(
      aes(
        x = .data$term,
        y = .data$PD_change * 100,
        fill = .data$PD_change * 100
      )
    ) +
    geom_col(position = "dodge") +
    scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red") +
    labs(
      x = "Maturity",
      y = "PD change in % points",
      title = "PD change by company, sector",
      subtitle = shock_year
    ) +
    facet_wrap(.data$company_name ~ .data$ald_sector, scales = "free") +
    r2dii.plot::theme_2dii() +
    theme(
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(2, 2, 14, 2)
      )
    )
}
