#' Extend the scenario pathways based on the fair share approach (now known as
#' market share approach).  We first ensure that all scenarios are extended to
#' the same point in time (start of the analysis plus forecast time frame.
#' Then we take the first value of the production on company level as the start
#' value and from the second year onward, we calculate the level of production
#' for a company under the scenarios at hand by applying the relative changes
#' for the respective years to the absolute values of the previous year.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param scenario_data A data frame containing scenario data for the specified
#'   parameters of the analysis, including the business as usual and target
#'   scenarios, the relevant scenario geography and time frame for each of the
#'   technologies.
#' @param start_analysis Numeric. A vector of length 1 indicating the start
#'   year of the analysis.
#' @param end_analysis Numeric. A vector of length 1 indicating the end
#'   year of the analysis.
#' @param time_frame Numeric. A vector of length 1 indicating the number of years
#'   for which forward looking production data is considered.
extend_scenario_trajectory <- function(data,
                                       scenario_data = NULL,
                                       start_analysis = NULL,
                                       end_analysis = NULL,
                                       time_frame = NULL) {
  force(data)
  scenario_data %||% stop("Must provide input for 'scenario_data'", call. = FALSE)
  start_analysis %||% stop("Must provide input for 'start_analysis'", call. = FALSE)
  end_analysis %||% stop("Must provide input for 'end_analysis'", call. = FALSE)
  time_frame %||% stop("Must provide input for 'time_frame'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "year", "investor_name", "portfolio_name", "equity_market", "ald_sector",
      "technology", "scenario", "allocation", "scenario_geography",
      "plan_tech_prod", "plan_carsten", "scen_tech_prod", "plan_sec_prod",
      "plan_sec_carsten", "id", "company_name"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c(
      "source", "technology", "scenario_geography", "ald_sector", "units",
      "scenario", "year", "direction", "fair_share_perc"
    )
  )

  data <- data %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_geography,
      .data$allocation, .data$year, .data$scenario, .data$plan_tech_prod,
      .data$scen_tech_prod
    ) %>%
    dplyr::filter(.data$year <= .env$start_analysis + .env$time_frame) %>%
    tidyr::complete(
      year = seq(.env$start_analysis, .env$end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "investor_name", "portfolio_name", "id", "company_name", "ald_sector",
            "technology", "scenario", "allocation", "scenario_geography"
          )
        )
      )
    )

  data <- data %>%
    # ADO 2393 - The join cols should be extended to cover the source
    dplyr::inner_join(
      scenario_data,
      by = c("ald_sector", "technology", "scenario_geography", "scenario", "year")
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "year", "investor_name", "portfolio_name", "id", "company_name",
        "ald_sector", "technology", "scenario", "allocation",
        "scenario_geography", "source", "units"
      )
    )

  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_geography,
      .data$allocation, .data$scenario
    ) %>%
    # TODO: since the start value for plan_tech_prod and scen_tech_prod is the same per definition,
    # it would be less error prone to fill the first value of scen_tech_prod with the prodcution value
    # and extrapolate everything else from the dedicated scenario file
    dplyr::mutate(
      scen_tech_prod = dplyr::if_else(
        .data$year > .env$start_analysis,
        dplyr::first(.data$scen_tech_prod) * (1 + .data$fair_share_perc),
        .data$scen_tech_prod
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(
        "investor_name", "portfolio_name", "id", "company_name", "year",
        "scenario_geography", "ald_sector", "technology", "plan_tech_prod"
      ),
      names_from = .data$scenario,
      values_from = .data$scen_tech_prod
    ) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$scenario_geography, .data$ald_sector, .data$technology, .data$year
    )
}
