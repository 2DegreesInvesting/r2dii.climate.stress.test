#' Display the annual pd changes per shock year and technology.
#'
#' @param data A data.frame that contains Merton output data.
#' @param shock_year_filter Numeric. Vector that specifies the respective start
#'   years of different policy shocks.
#' @param geography_filter Character. A vector containing the geographical
#'   specification of the scenario
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
annual_pd_change_sector_shock_year <- function(data,
                                               shock_year_filter = NULL,
                                               geography_filter = NULL) {
  force(data)

  shock_year_filter %||% stop("Must provide input for 'shock_year_filter'", call. = FALSE)
  geography_filter %||% stop("Must provide input for 'geography_filter'", call. = FALSE)

  data_has_expected_columns <- all(c(
    "scenario_name", "ald_sector", "scenario_geography", "year", "PD_change"
  )
  %in% colnames(data))
  stopifnot(data_has_expected_columns)

  shock_year_filter <- stringr::str_c("Carbon balance ", shock_year_filter)

  plot <- data %>%
    dplyr::filter(
      .data$scenario_name %in% shock_year_filter,
      .data$scenario_geography %in% geography_filter
    ) %>%
    dplyr::mutate(
      scenario_name = stringr::str_replace(
        .data$scenario_name,
        "Carbon balance",
        "Shock year"
      )
    ) %>%
    ggplot(
      aes(
        # TODO: must display full years
        x = .data$year,
        y = .data$PD_change * 100,
        fill = .data$PD_change * 100
      )
    ) +
    geom_col(position = "dodge") +
    scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red") +
    labs(
      x = "Year",
      y = "PD change in % points",
      title = "Annual PD change by shock year, sector"
    ) +
    facet_grid(
      rows = vars(.data$scenario_name),
      cols = vars(.data$ald_sector),
      scales = "free"
    ) +
    r2dii.plot::theme_2dii() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}
