#' Translate power capacity to power generation. Units of generated power are
#' assumed to be sold and hence get priced in the net profit calculations.
#' This also entails converting MWh into MW per year, since we calculate yearly
#' profits.
#'
#' @param data A data frame filtered and wrangled company level production
#'   forecasts (of the companies in the portfolio). Usually based on PACTA output.
#' @param capacity_factors_power A data frame containing capacity factors to
#'   translate company level power capacity to units sold. Contains information
#'   on the technology (pwer sector only) and scenario_geography levels.

convert_cap_to_generation <- function(data,
                                      capacity_factors_power = NULL) {
  force(data)
  capacity_factors_power %||% stop("Must provide input for 'capacity_factors_power'", call. = FALSE)

  data_has_expected_columns <- all(
    c(
      "year", "investor_name", "portfolio_name", "equity_market", "ald_sector",
      "technology", "scenario", "allocation", "scenario_geography",
      "plan_tech_prod", "plan_carsten", "scen_tech_prod", "plan_sec_prod",
      "plan_sec_carsten", "id", "company_name"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  capacity_factors_has_expected_columns <- all(
    c(
      "technology", "capacity_factor", "scenario_geography"
    ) %in% colnames(capacity_factors_power)
  )
  stopifnot(capacity_factors_has_expected_columns)

  data <- data %>%
    dplyr::left_join(
      capacity_factors_power,
      by = c("technology", "scenario_geography")
    )

  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::mutate(
      plan_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$plan_tech_prod * .data$capacity_factor * .env$hours_to_year,
        .data$plan_tech_prod
      ),
      scen_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$scen_tech_prod * .data$capacity_factor * .env$hours_to_year,
        .data$scen_tech_prod
      ),
      scenario_geography = .data$scenario_geography
    ) %>%
    dplyr::select(-.data$capacity_factor)
}

#' Translate power capacity to power generation
#'
#' Units of generated power are assumed to be sold and hence get priced in the
#' net profit calculations. This also entails converting MWh into MW per year,
#' since we calculate yearly profits. Note: For use in webscripts
#' [convert_cap_to_generation()] is used currently, which only distinguishes
#' capacity factor by technology and scenario_geography, whereas this function
#' distinguishes further by year and scenario. Also note that for generation of
#' variable `plan_tech_prod` (planned capacity) capacity factors from baseline
#' scenario are used.
#'
#' @param data A data frame filtered and wrangled company level production
#'   forecasts (of the companies in the portfolio). Usually based on PACTA
#'   output.
#' @param capacity_factors_power A data frame containing capacity factors to
#'   translate company level power capacity to units sold. Contains information
#'   on the technology (pwoer sector only) and scenario_geography levels.
#' @param baseline_scenario String holding name of baseline scenario.
convert_power_cap_to_generation <- function(data,
                                            capacity_factors_power = NULL,
                                            baseline_scenario) {
  force(data)
  capacity_factors_power %||% stop("Must provide input for 'capacity_factors_power'", call. = FALSE)

  data_has_expected_columns <- all(
    c(
      "year", "investor_name", "portfolio_name", "equity_market", "ald_sector",
      "technology", "scenario", "allocation", "scenario_geography",
      "plan_tech_prod", "plan_carsten", "scen_tech_prod", "plan_sec_prod",
      "plan_sec_carsten", "id", "company_name"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  capacity_factors_has_expected_columns <- all(
    c(
      "technology", "capacity_factor", "scenario_geography", "year", "scenario"
    ) %in% colnames(capacity_factors_power)
  )
  stopifnot(capacity_factors_has_expected_columns)

  # helper data set for calculation of planned capacity that assumes baseline scenario
  capacity_factors_power_baseline <- capacity_factors_power %>%
    dplyr::filter(.data$scenario == baseline_scenario) %>%
    dplyr::mutate(capacity_factor_plan = .data$capacity_factor) %>%
    dplyr::select(-scenario)

  # Left join is applied since only rows in data from ald_sector power will
  # have matching rows in capacity_factors_power
  data <- data %>%
    dplyr::left_join(
      capacity_factors_power,
      by = c("technology", "scenario_geography", "year", "scenario")
    ) %>% dplyr::left_join(
      capacity_factors_power_baseline,
      by = c("technology", "scenario_geography", "year")
    )

  hours_to_year <- 24 * 365

  data <- data %>%
    dplyr::mutate(
      plan_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$plan_tech_prod * .data$capacity_factor_plan * .env$hours_to_year,
        .data$plan_tech_prod
      ),
      scen_tech_prod = dplyr::if_else(
        .data$ald_sector == "Power",
        .data$scen_tech_prod * .data$capacity_factor * .env$hours_to_year,
        .data$scen_tech_prod
      ),
      scenario_geography = .data$scenario_geography
    ) %>%
    dplyr::select(-.data$capacity_factor_plan, -.data$capacity_factor)
}
