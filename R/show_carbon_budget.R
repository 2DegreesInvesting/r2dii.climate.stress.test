#' Display the carbon budget per technology over time
#'
#' @param data A dataframe that contains the QA annual profits data frame for an
#'   asset type.
#' @param scenarios A dataframe that contains the fully prepared scenario data
#'   used in the analysis.
#' @param target_scenario Character. A vector of length one containing the name
#'   of the target scenario against which to compare the carbon budget.
#' @param scenario_name_qa Character. A vector of length one containing the name of
#'   the transition scenario to inspect.
#' @param cumulative Logical. A vector of length one, indicating whether the
#'   output should be an overview of changes in the carbon budget per technology
#'   over the time frame of the analysis (FALSE) or a summary that shows the
#'   overall deviation from the carbon budget by technology (TRUE).
#'
#' @family qa graphing functions
#'
#' @return ggplot object
show_carbon_budget <- function(data,
                               scenarios = NULL,
                               target_scenario = NULL,
                               scenario_name_qa = "Carbon balance 2030",
                               cumulative = FALSE) {
  scenarios <- scenarios %||% .env$scenario_data
  target_scenario <- target_scenario %||% .env$scenario_to_follow_ls

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "id", "company_name", "scenario_name",
      "year", "scenario_geography", "ald_sector", "technology", "late_sudden",
      "sector_unit_ds", "year_of_shock"
    )
  )

  validate_data_has_expected_cols(
    data = scenarios,
    expected_columns = c(
      "source", "scenario_geography", "scenario", "ald_sector", "technology",
      "direction"
    )
  )

  target_scenario_is_character <- is.character(target_scenario) &
    length(target_scenario) == 1
  stopifnot(target_scenario_is_character)

  tech_direction <- scenarios %>%
    dplyr::filter(.data$scenario == .env$target_scenario) %>%
    dplyr::select(
      .data$source, .data$scenario_geography, .data$scenario,
      .data$ald_sector, .data$technology, .data$direction
    ) %>%
    dplyr::distinct_all()


  data <- data %>%
    dplyr::mutate(
      target_scenario = !!rlang::sym(target_scenario)
    ) %>%
    dplyr::group_by(
      .data$scenario_name, .data$year, .data$scenario_geography,
      .data$ald_sector, .data$technology, .data$sector_unit_ds,
      .data$year_of_shock
    ) %>%
    dplyr::summarise(
      late_sudden = sum(.data$late_sudden, na.rm = TRUE),
      target_scenario = sum(.data$target_scenario, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  # ADO 1945 - fix qa script, then check if left_join can be replaced
  data <- data %>%
    dplyr::filter(.data$scenario_name == .env$scenario_name_qa) %>%
    dplyr::left_join(
      tech_direction,
      by = c("scenario_geography", "ald_sector", "technology")
    ) %>%
    dplyr::mutate(
      net_budget_surplus = .data$late_sudden - .data$target_scenario,
      green = dplyr::if_else(
        (.data$direction == "increasing" & .data$net_budget_surplus >= 0) |
          (.data$direction == "declining" & .data$net_budget_surplus <= 0),
        TRUE,
        FALSE
      )
    )

  if (identical(cumulative, FALSE)) {
    shock_year <- unique(data$year_of_shock)

    carbon_budget <- data %>%
      ggplot(
        aes(
          x = .data$year,
          y = .data$net_budget_surplus,
          fill = .data$green
        )
      ) +
      geom_col(alpha = .5) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = shock_year, colour = "red", alpha = 0.5) +
      labs(y = "Net carbon budget surplus (production units)", colour = "Compensating") +
      expand_limits(y = 0) +
      facet_wrap(vars(.data$technology), scales = "free") +
      theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  } else {
    summary_data <- data %>%
      dplyr::group_by(.data$scenario_name, .data$technology, .data$direction) %>%
      dplyr::summarise(
        sum_ls = sum(.data$late_sudden, na.rm = TRUE),
        sum_target = sum(.data$target_scenario, na.rm = TRUE),
        sum_net_budget_surplus = .data$sum_ls - .data$sum_target,
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct_all() %>%
      dplyr::mutate(
        green = dplyr::if_else(
          (.data$direction == "increasing" & .data$sum_net_budget_surplus >= 0) |
            (.data$direction == "declining" & .data$sum_net_budget_surplus <= 0),
          TRUE,
          FALSE
        )
      )

    carbon_budget <- summary_data %>%
      ggplot(
        aes(
          x = .data$technology,
          y = .data$sum_net_budget_surplus / .data$sum_target,
          fill = .data$green
        )
      ) +
      geom_col(alpha = .5) +
      geom_hline(yintercept = 0) +
      labs(y = "Overall deviation from carbon budget (percentage of production units)", colour = "Compensating") +
      expand_limits(y = c(-1, 1)) +
      theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  }
}
