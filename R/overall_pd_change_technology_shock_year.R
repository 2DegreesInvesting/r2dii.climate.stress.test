#' Display the overall pd changes per maturity, shock year and technology.
#'
#' @param data A data.frame that contains Merton output data.
#' @param scenario_filter Character. A vector of scenario names, that determines
#'   for which shock years to display the graph.
#' @param geography_filter Character. A vector containing the geographical
#'   specification of the scenario
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
overall_pd_change_technology_shock_year <- function(data,
                                                    scenario_filter = NULL,
                                                    geography_filter = NULL) {
  force(data)

  scenario_filter %||% stop("Must provide input for 'scenario_filter'", call. = FALSE)
  geography_filter %||% stop("Must provide input for 'geography_filter'", call. = FALSE)

  data_has_expected_columns <- all(c(
    "scenario_name", "ald_sector", "technology", "scenario_geography", "term",
    "PD_change_late_sudden"
  )
  %in% colnames(data))
  stopifnot(data_has_expected_columns)

  plot <- data %>%
    dplyr::filter(
      .data$scenario_name %in% .env$scenario_filter,
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
        x = .data$term,
        y = .data$PD_change_late_sudden * 100,
        fill = .data$PD_change_late_sudden * 100
      )
    ) +
    geom_col(position = "dodge") +
    scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red") +
    labs(
      x = "Maturity",
      y = "PD change in % points",
      title = "Overall PD change by sector, maturity & shock year"
    ) +
    facet_grid(
      rows = vars(.data$scenario_name),
      cols = vars(.data$ald_sector, .data$technology),
      scales = "free"
    ) +
    r2dii.plot::theme_2dii()
}
