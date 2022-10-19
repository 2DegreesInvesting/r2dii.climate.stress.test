#' Extend the scenario pathways based on the fair share approach (now known as
#' market share approach).  We first ensure that all company production plans
#' are kept from the start year until the end of the forecast period.
#' We then extend the scenario trajectories by multiplying the start value of
#' the production with the relative change in each year/scenario combination.
#' Contrary to the old version, this implements company targets based on the
#' SMSP for increasing technologies and TMSR for decreasing ones.
#' Companies that get production targets below 0 or that show a pattern of
#' phasing out a technology within the forecast period, will get 0 scenario
#' targets.
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
#' @param time_frame Numeric. A vector of length 1 indicating the number of
#'   years for which forward looking production data is considered.
#' @param target_scenario Character. A vector of length 1 indicating target
#'   scenario

#' @noRd
extend_scenario_trajectory <- function(data,
                                       scenario_data,
                                       start_analysis,
                                       end_analysis,
                                       time_frame,
                                       target_scenario) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "id", "company_name", "year", "ald_sector", "technology",
      "scenario_geography", "plan_tech_prod", "plan_emission_factor",
      "plan_sec_prod"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_data,
    expected_columns = c(
      "technology", "scenario_geography", "ald_sector", "units",
      "scenario", "year", "direction", "fair_share_perc"
    )
  )

  data <- data %>%
    summarise_production_technology_forecasts(
      start_analysis = start_analysis,
      time_frame = time_frame
    ) %>%
    identify_technology_phase_out() %>%
    extend_to_full_analysis_timeframe(
      start_analysis = start_analysis,
      end_analysis = end_analysis
    )

  data <- data %>%
    dplyr::inner_join(
      scenario_data,
      by = c("ald_sector", "technology", "scenario_geography", "year")
    ) %>%
    report_all_duplicate_kinds(
      composite_unique_cols = c(
        "year", "id", "company_name", "ald_sector", "technology", "scenario",
        "scenario_geography", "units"
      )
    )

  data <- data %>%
    summarise_production_sector_forecasts()

  data <- data %>%
    apply_scenario_targets() %>%
    handle_phase_out_and_negative_targets()

  data <- data %>%
    calculate_proximity_to_target(
      start_analysis = start_analysis,
      time_frame = time_frame,
      target_scenario = target_scenario
    )

  data <- data %>%
    tidyr::pivot_wider(
      id_cols = c(
        "id", "company_name", "year", "scenario_geography", "ald_sector",
        "technology", "plan_tech_prod", "phase_out", "emission_factor",
        "proximity_to_target", "direction"
      ),
      names_from = "scenario",
      values_from = "scen_tech_prod"
    ) %>%
    dplyr::arrange(
      .data$id, .data$company_name, .data$scenario_geography, .data$ald_sector,
      .data$technology, .data$year
    )

  return(data)
}

#' Summarise the forecasts for company-tech level production within the five
#' year time frame
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param start_analysis start of the analysis
#' @param time_frame number of years with forward looking production data
#' @noRd
summarise_production_technology_forecasts <- function(data,
                                                      start_analysis,
                                                      time_frame) {
  data <- data %>%
    dplyr::select(
      dplyr::all_of(c("id", "company_name", "ald_sector", "technology",
      "scenario_geography", "year", "plan_tech_prod",
      "plan_emission_factor")
      )
    ) %>%
    dplyr::filter(.data$year <= .env$start_analysis + .env$time_frame) %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::arrange(
      .data$id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography, .data$year
    ) %>%
    dplyr::mutate(
      # Initial value is identical between production and scenario target,
      # can thus be used for both
      initial_technology_production = dplyr::first(.data$plan_tech_prod),
      final_technology_production = dplyr::last(.data$plan_tech_prod),
      sum_production_forecast = sum(.data$plan_tech_prod, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(data)
}

#' Identify which company technology combination is a phase out and mark as such
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
identify_technology_phase_out <- function(data) {
  data <- data %>%
    dplyr::mutate(
      phase_out = dplyr::if_else(
        .data$final_technology_production == 0 &
          .data$sum_production_forecast > 0,
        TRUE,
        FALSE
      )
    )
}

#' Extend the dataframe containing the production and production summaries to
#' cover the whole timeframe of the analysis, filling variables downwards where
#' applicable.
#'
#' @param data A data frame containing the production forecasts of companies,
#'   the summaries fo their forecasts and the phase out indicator.
#' @param start_analysis start of the analysis
#' @param end_analysis end of the analysis
#' @noRd
extend_to_full_analysis_timeframe <- function(data,
                                              start_analysis,
                                              end_analysis) {
  data <- data %>%
    tidyr::complete(
      year = seq(.env$start_analysis, .env$end_analysis),
      tidyr::nesting(
        !!!rlang::syms(
          c(
            "id", "company_name", "ald_sector", "technology", "scenario_geography"
          )
        )
      )
    ) %>%
    dplyr::arrange(
      .data$id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography, .data$year
    ) %>%
    tidyr::fill(
      dplyr::all_of(c("initial_technology_production",
               "final_technology_production",
               "phase_out",
               "plan_emission_factor")
      )
    ) %>%
    dplyr::rename(
      emission_factor = "plan_emission_factor"
    )

  return(data)
}

#' Summarise the forecasts for company-sector level production within the five
#' year time frame
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
summarise_production_sector_forecasts <- function(data) {
  data <- data %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$scenario,
      .data$scenario_geography, .data$units, .data$year
    ) %>%
    dplyr::mutate(
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE)
    ) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$scenario,
      .data$scenario_geography, .data$units
    ) %>%
    dplyr::mutate(
      # first year plan and scenario values are equal by construction,
      # can thus be used for production and target
      initial_sector_production = dplyr::first(.data$plan_sec_prod)
    ) %>%
    dplyr::ungroup()
}

#' Apply TMSR/SMSP scenario targets based on initial technology or sector
#' production and type of technology
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
apply_scenario_targets <- function(data) {
  data <- data %>%
    dplyr::mutate(
      scen_tech_prod = dplyr::if_else(
        .data$direction == "declining",
        .data$initial_technology_production * (1 + .data$fair_share_perc), # tmsr
        .data$initial_technology_production + (.data$initial_sector_production * .data$fair_share_perc) # smsp
      )
    )

  return(data)
}

#' Set scenario targets to zero where companies phase out a technology or the
#' extension of the technology leads to negative values
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @noRd
handle_phase_out_and_negative_targets <- function(data) {
  data <- data %>%
    dplyr::mutate(
      scen_tech_prod = dplyr::case_when(
        .data$scen_tech_prod < 0 ~ 0,
        .data$phase_out == TRUE ~ 0,
        TRUE ~ .data$scen_tech_prod
      )
    )
}

#' Calculate the ratio of the required change in technology that each company
#' has achieved per technology at the end of the production forecast period.
#' This ratio will later serve to adjust the net profit margin for companies
#' that have not built out enough production capacity in increasing technologies
#' and hence need to scale up production to compensate for their lag in buildout.
#'
#' @param data A data frame containing the production forecasts of companies
#'   (in the portfolio). Pre-processed to fit analysis parameters and after
#'   conversion of power capacity to generation.
#' @param start_analysis Numeric. A vector of length 1 indicating the start
#'   year of the analysis.
#' @param time_frame Numeric. A vector of length 1 indicating the number of
#'   years for which forward looking production data is considered.
#' @param target_scenario Character. A vector of length 1 indicating target
#'   scenario
#'
#' @noRd
calculate_proximity_to_target <- function(data,
                                          start_analysis,
                                          time_frame,
                                          target_scenario) {
  production_changes <- data %>%
    dplyr::filter(
      dplyr::between(
        .data$year, .env$start_analysis, .env$start_analysis + .env$time_frame
      ),
      .data$scenario == .env$target_scenario
    ) %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      required_change = .data$scen_tech_prod - .data$initial_technology_production,
      realised_change = .data$plan_tech_prod - .data$initial_technology_production
    ) %>%
    dplyr::summarise(
      sum_required_change = sum(.data$required_change, na.rm = TRUE),
      sum_realised_change = sum(.data$realised_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ratio_realised_required = .data$sum_realised_change / .data$sum_required_change,
      proximity_to_target = dplyr::case_when(
        .data$ratio_realised_required < 0 ~ 0,
        .data$ratio_realised_required > 1 ~ 1,
        TRUE ~ .data$ratio_realised_required
      )
    ) %>%
    dplyr::select(
      -dplyr::all_of(c(
        "sum_required_change", "sum_realised_change",
        "ratio_realised_required")
      )
    )

  data <- data %>%
    dplyr::inner_join(
      production_changes,
      by = c(
        "id", "company_name", "ald_sector", "technology", "scenario_geography"
      )
    )
}
