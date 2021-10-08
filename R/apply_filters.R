#' Filter input company or portfolio data to match model specifications and
#' parameters. Companies with zero production in the start year are removed as
#' they will lead to errors in calculations down the line.
#'
#' @param data A dataframe containing the production forecasts of companies
#'   (in the portfolio). Usually a PACTA output.
#' @param investor Character. A vector of length 1 containing the name of the
#'   investor for whom to run the analysis.
#' @param sectors Character. A vector containing the sectors to include in the
#'   analysis. In most cases this will be a subset of PACTA sectors.
#' @param technologies Character. A vector containing the technologies to
#'   include in the analysis. In most cases this will be a subset of PACTA
#'   technologies.
#' @param scenario_geography_filter Character. A vector of length 1 that
#'   indicates which geographic scenario to apply in the analysis.
#' @param scenarios Chracter. A vector containing the unique names of the
#'   scenarios to include in the analysis. This refers to baseline, target and
#'   aligned scenarios.
#' @param allocation_method Character. A vector of length 1 indicating the
#'   set of PACTA data to be used in the analysis, based on the choice of an
#'   allocation rule.
#' @param start_analysis Numeric. A vector of length 1 indicating the start
#'   year of the analysis.

apply_filters <- function(data,
                          investor = NULL,
                          sectors = NULL,
                          technologies = NULL,
                          scenario_geography_filter = NULL,
                          scenarios = NULL,
                          allocation_method = NULL,
                          start_analysis = NULL) {
  force(data)
  investor %||% stop("Must provide input for 'investor'", call. = FALSE)
  sectors %||% stop("Must provide input for 'sectors'", call. = FALSE)
  technologies %||% stop("Must provide input for 'technologies'", call. = FALSE)
  scenario_geography_filter %||% stop("Must provide input for 'scenario_geography_filter'", call. = FALSE)
  scenarios %||% stop("Must provide input for 'scenarios'", call. = FALSE)
  allocation_method %||% stop("Must provide input for 'allocation_method'", call. = FALSE)
  start_analysis %||% stop("Must provide input for 'start_analysis'", call. = FALSE)

  data_has_expected_columns <- all(
    c(
      "year", "investor_name", "portfolio_name", "equity_market", "ald_sector",
      "technology", "scenario", "allocation", "scenario_geography",
      "plan_tech_prod", "plan_carsten", "scen_tech_prod", "plan_sec_prod",
      "plan_sec_carsten"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  if (!"id" %in% names(data)) {
    data$id <- "PortfolioLevel"
  }

  if (!"company_name" %in% names(data)) {
    data$company_name <- "PortfolioLevel"
  }

  data <- data %>%
    dplyr::filter(
      .data$ald_sector %in% .env$sectors,
      .data$technology %in% .env$technologies,
      .data$scenario_geography == .env$scenario_geography_filter,
      .data$scenario %in% .env$scenarios,
      .data$investor_name == .env$investor,
      .data$allocation == .env$allocation_method,
      !is.na(.data$plan_tech_prod)
    )

  remove <- data %>%
    dplyr::filter(
      .data$plan_tech_prod == 0,
      .data$year == .env$start_analysis
    ) %>%
    dplyr::mutate(
      key = paste(.data$id, .data$ald_sector, .data$technology, .data$scenario_geography)
    ) %>%
    dplyr::distinct(.data$key)

  data <- data %>%
    dplyr::mutate(
      key = paste(.data$id, .data$ald_sector, .data$technology, .data$scenario_geography)
    ) %>%
    dplyr::filter(!.data$key %in% unique(remove$key)) %>%
    dplyr::select(-.data$key)
}
