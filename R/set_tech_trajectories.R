#' Defines which scenario values to use for the baseline trajectory in the
#' stress test.
#'
#' @description
#' Picks the corresponding values from the original scenario
#' column indicated in the input and has the option to include PACTA based
#' production forecast for the first few years of the baseline
#' trajectory. If included, the trajectory after the end of the production
#' forecast is offset by the initial production forecast so that the
#' remainder of the baseline trajectory now is a parallel shift of the
#' original scenario values. If not included, the trajectories replicate
#' externally provided scenario trajectories.
#' Trajectories are furthermore differentiated by scenario_geography, if
#' multiple are passed.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/ald_business_unit level.
#'
#' @param data A dataframe that contains scenario trajectories by ald_business_unit
#'   until 2040 for all the scenarios included in the analysis and
#'   production build out plans by ald_business_unit or company and ald_business_unit,
#'   usually for 5 years, based on PACTA results.
#' @param baseline_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline ald_business_unit trajectories.
#'
#' @family scenario definition
#'
#' @return dataframe.
set_baseline_trajectory <- function(data,
                                    baseline_scenario) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography",
      "plan_tech_prod", "emission_factor", baseline_scenario
    )
  )


  data <- data %>%
    dplyr::mutate(
      scen_to_follow = !!rlang::sym(baseline_scenario),
      # compute the scenario change derivative, where the input production is NA
      scenario_change = dplyr::if_else(
        is.na(.data$plan_tech_prod),
        .data$scen_to_follow - dplyr::lag(.data$scen_to_follow, default = 0),
        0
      ),
      baseline = .data$plan_tech_prod
    ) %>%
    # Fill the baseline/input production with the latest non-NA value
    tidyr::fill(.data$baseline, .direction = "down") %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      # compute per group the cumulative sum of the scenario change derivatives at each year
      # add the cumsum to the input production , so that the latest non-NA value is incremented
      # by the cumulative sum of all scenario change local derivative value
      baseline=.data$baseline + cumsum(.data$scenario_change)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      baseline = dplyr::if_else(.data$baseline < 0, 0, .data$baseline),
      baseline_change=.data$scenario_change
    ) %>%
    dplyr::select(-dplyr::all_of(c("scenario_change", "scen_to_follow")))

  return(data)
}


#' Calculates the production trajectory in case the initial production plans
#' are to be included.
#'
#' @description
#' In this case, the production trajectory between the
#' end of the actual production forecast and the end of the analysis need to
#' be recalculated. They follow the initial baseline scenario, offset by the
#' production plans.
#'
#' @param planned_prod Numeric vector that includes the production plans for a
#'   company or (aggregated) ald_business_unit to be included. The length of the vector
#'   for each company is from the start year of the analysis to the end year of
#'   the analysis, which means that in most cases, this vector will include NAs
#'   after the final forecast year. This usually comes from a PACTA analysis.
#' @param change_scen_prod Numeric vector that contains the absolute changes of
#'   the production trajectories according to the baseline scenario. After the
#'   end of the production forecast period (i.e. the first NA in planned_prod),
#'   the planned production is extended until the end of the analysis period
#'   using these absolute changes. This results in an offset that is a parallel
#'   shift relative to the original baseline scenario.
#'
#' @family scenario definition
#'
#' @return numeric vector
calc_future_prod_follows_scen <- function(planned_prod = .data$plan_tech_prod,
                                          change_scen_prod = .data$scenario_change) {
  first_production_na <- which(is.na(planned_prod))[1]

  for (i in seq(first_production_na, length(planned_prod))) {
    planned_prod[i] <- planned_prod[i - 1] + change_scen_prod[i]
  }

  planned_prod
}

#' Defines which scenario values to use for the late & sudden trajectory in the
#' stress test.
#'
#' @description
#' Picks the corresponding values from the original scenario
#' column indicated in the input and has the option to include PACTA based
#' production forecast for the first few years of the late & sudden
#' trajectory. Similarly, it is possible to define another input scenario
#' in case the company is already aligned after the production forecast.
#' If the production forecast is included, the trajectory after the end of
#' the production forecast is offset by the initial production forecast
#' so that the remainder of the late & sudden trajectory now is a parallel
#' shift of the original scenario values. If not included, the trajectories
#' replicate externally provided scenario trajectories at least until the
#' year of the policy shock.
#' Trajectories are calculated for each company by sector, scenario_geography,
#' ald_business_unit, year.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/ald_business_unit level.
#' @inheritParams report_company_drops
#' @param data A dataframe that contains the scenario data prepared until the
#'   step after the baseline trajectories are calculated.
#' @param target_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories.
#' @param shock_scenario A dataframe that contains information about the
#'   transition scenario, specifically the shock year and, duration of the
#'   shock and the name of the shock scenario
#' @param target_scenario_aligned Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden ald_business_unit trajectories in case the company is aligned after
#'   the forecast period.
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @param analysis_time_frame Numeric. A vector of length 1 indicating the number
#'   of years for which forward looking production data is considered.
#'
#' @family scenario definition
#'
#' @return data frame
set_trisk_trajectory <- function(data,
                                 target_scenario,
                                 shock_scenario,
                                 target_scenario_aligned,
                                 start_year,
                                 end_year,
                                 analysis_time_frame,
                                 log_path) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography",
      "plan_tech_prod", "baseline",
      target_scenario, target_scenario_aligned
    )
  )

  validate_data_has_expected_cols(
    data = shock_scenario,
    expected_columns = c(
      "year_of_shock", "duration_of_shock", "scenario_name"
    )
  )

  scenario_name <- shock_scenario$scenario_name
  year_of_shock <- shock_scenario$year_of_shock
  duration_of_shock <- shock_scenario$duration_of_shock

  data <- data %>%
    dplyr::mutate(
      late_sudden = .data$plan_tech_prod,
      scen_to_follow = !!rlang::sym(target_scenario),
      scen_to_follow_aligned = !!rlang::sym(target_scenario_aligned),
      scenario_change =
        dplyr::if_else(
          is.na(.data$late_sudden),
          .data$scen_to_follow - dplyr::lag(.data$scen_to_follow),
          0
        ),
      scenario_change_aligned =
        dplyr::if_else(
          is.na(.data$late_sudden),
          .data$scen_to_follow_aligned - dplyr::lag(.data$scen_to_follow_aligned),
          0
        ),
      scenario_change_baseline = dplyr::if_else(
        is.na(.data$late_sudden),
        .data$baseline - dplyr::lag(.data$baseline),
        0
      )
      )



    data <- data %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      overshoot_direction = rep(
        dplyr::if_else(
          .data$scen_to_follow[1] - .data$scen_to_follow[length(.data$scen_to_follow)] > 0,
          "Decreasing",
          "Increasing"
        ),
        dplyr::n()
      ),
      late_sudden = calc_late_sudden_traj(
        start_year = start_year,
        end_year = end_year,
        year_of_shock = year_of_shock,
        duration_of_shock = duration_of_shock,
        scen_to_follow = .data$scen_to_follow,
        planned_prod = .data$plan_tech_prod,
        late_sudden = .data$late_sudden,
        scenario_change = .data$scenario_change,
        scenario_change_baseline = .data$scenario_change_baseline,
        scenario_change_aligned = .data$scenario_change_aligned,
        overshoot_direction = .data$overshoot_direction[1],
        time_frame = .env$analysis_time_frame
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::all_of(c(
        "scen_to_follow",
        "scenario_change",
        "scenario_change_baseline",
        "scenario_change_aligned"
      ))
    ) %>%
    dplyr::mutate(scenario_name = .env$scenario_name)

  data <- filter_negative_late_and_sudden(data, log_path = log_path)

  return(data)
}


#' Calculate how the production trajectory for a company/ald_business_unit changes
#' after the policy shock hits.
#'
#' @description
#' Prior to the shock, this will keep the
#' trajectory untouched, i.e., the trajectory follows baseline up until the
#' shock. After the shock hits, the development depends on whether or not
#' the company/ald_business_unit is already aligned and on what type of calculation
#' is selected for the shock. Overall the outcome should lead the
#' company/ald_business_unit to stay within the bounds of the carbon budget, if the
#' method applied is the overshoot/carbon budget method.
#'
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @param year_of_shock Numeric. A numeric vector of length 1 that contains the
#'   year in which the policy shock first hits.
#' @param duration_of_shock Numeric. A numeric vector of length 1 that contains
#'   the duration of the shock in years. I.e. the number of years it takes until
#'   the trajectory of the company/sector reaches a new equilibrium pathway.
#' @param shock_strength Numeric. A numeric vector that contains the shock
#'   size for the given company/ald_business_unit at hand, in case shock size is not
#'   calculated endogenously by using the overshoot/carbon budget method.
#'   TODO: (move to data argument)
#' @param scen_to_follow Numeric. A numeric vector that contains the production
#'   trajectory of the scenario indicated to use as the target for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param planned_prod Numeric vector that includes the production plans for a
#'   company or (aggregated) ald_business_unit to be included. The length of the vector
#'   for each company is from the start year of the analysis to the end year of
#'   the analysis, which means that in most cases, this vector will include NAs
#'   after the final forecast year. This usually comes from a PACTA analysis.
#'   TODO: (move to data argument)
#' @param late_and_sudden Numeric. A numeric vector that contains the
#'   late & sudden production trajectory for the company/ald_business_unit at hand.
#'   Before applying the shock, this follows the baseline scenario.
#'   TODO: (move to data argument)
#' @param scenario_change Numeric. A numeric vector that contains the
#'   absolute changes of the target scenario in yearly steps for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_baseline Numeric. A numeric vector that contains the
#'   absolute changes of the baseline scenario in yearly steps for the
#'   company/ald_business_unit at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_aligned Numeric. A numeric vector that contains the
#'   absolute changes of the aligned target scenario in yearly steps for the
#'   company/ald_business_unit at hand, in case the company/ald_business_unit is aligned
#'   with the target after the forecast period.
#'   TODO: (move to data argument)
#' @param overshoot_direction Character. A character vector that indicates if
#'   the ald_business_unit at hand is increasing or decreasing over the time frame of
#'   the analysis.
#'   TODO: (move to data argument)
#' @param time_frame Numeric. A vector of length 1 indicating the number of years
#'   for which forward looking production data is considered.
#'
#' @family scenario definition
#'
#' @return numeric vector
calc_late_sudden_traj <- function(start_year, end_year, year_of_shock, duration_of_shock,
                                  shock_strength, scen_to_follow, planned_prod, late_sudden,
                                  scenario_change, scenario_change_baseline, scenario_change_aligned,
                                  overshoot_direction, time_frame) {

  time_frame %||% stop("Must provide input for 'time_frame'", call. = FALSE)


  # calculate the position where the shock kicks in
  position_shock_year <- year_of_shock - start_year + 1

  na_range <- which(is.na(late_sudden[1:position_shock_year]))
  # first position for which future production is unknown
  first_production_na <- na_range[1]

  if (length(na_range) > 0) {
    # if this is true, then there are NA's in the period after the company prod
    # forecasts and the shock period
    # i.e. we need to fill the values between the last year we have production
    # forecasts, and the year of the shock
    # for example, if the shock year is 2026 and we have production forecasts
    # until 2024, we need to calculate L&S production for 2025 (we follow
    # baseline as it is the late & sudden scen)

    # Calculate the cumulative sum for the scenario_change_baseline
    # Update the late_and_sudden values
    late_sudden[first_production_na:position_shock_year] <-
      late_sudden[first_production_na - 1] +
      cumsum(scenario_change_baseline)[first_production_na:position_shock_year]
  }

  # integral/overshoot compensation method
  # If the company production plans are already aligned
  # we do not need to compensate production capacity, and set LS trajectory to follow
  # the scenario indicated as late & sudden aligned
  if (
    (overshoot_direction == "Decreasing" & sum(scen_to_follow[1:time_frame + 1]) < sum(late_sudden[1:time_frame + 1])) |
      (overshoot_direction == "Increasing" & sum(scen_to_follow[1:time_frame + 1]) > sum(late_sudden[1:time_frame + 1]))
  ) {

    x <- (
      sum(scen_to_follow) -
        sum(late_sudden[1:(position_shock_year - 1)]) -
        (end_year - year_of_shock + 1) * late_sudden[position_shock_year - 1]
    ) /
      (
        -sum(seq(1, end_year - year_of_shock + 1))
      )

    # add the absolute production increase/decrease for each year during
    # the shock period, capping at a 0 lower bound for production volume
    sequence_length <- seq(position_shock_year, length(scen_to_follow))
    late_sudden[sequence_length] <- pmax(
      late_sudden[position_shock_year - 1] - (sequence_length - position_shock_year + 1) * x,
      0
    )

  } else {
    # company plans are already aligned
    # no need for overshoot in production cap, set LS trajectory to follow
    # the scenario indicated as late & sudden aligned
    # negative production adjustment: if shock production goes below 0
    # then this and future production stays constant at 0.

    for (k in seq(first_production_na, length(scen_to_follow))) {
      late_sudden[k] <- late_sudden[k - 1] + scenario_change_aligned[k]
      if (late_sudden[k] < 0) {
        late_sudden[k:length(late_sudden)] <- 0
        break
      }
    }
  }
  return(late_sudden)
}

#' Remove negative late and sudden rows
#'
#' Function checks for negative values on variable late_and_sudden. All
#' ald_business_unit x company_name combinations holding >= 1 negative value are
#' removed.
#'
#' @inheritParams report_company_drops
#' @param data_with_late_and_sudden A tibble containing scenario data with
#'   projected late and sudden trajectory.
#'
#' @return Input tibble with potentially removed rows.
filter_negative_late_and_sudden <- function(data_with_late_and_sudden, log_path) {
  negative_late_and_sudden <- data_with_late_and_sudden %>%
    dplyr::filter(.data$late_sudden < 0) %>%
    dplyr::select(dplyr::all_of(c("company_name", "ald_business_unit"))) %>%
    dplyr::distinct()

  if (nrow(negative_late_and_sudden) > 0) {
    n_rows_before_removal <- nrow(data_with_late_and_sudden)

    data_with_late_and_sudden <-
      data_with_late_and_sudden %>%
      dplyr::anti_join(negative_late_and_sudden, by = c("company_name", "ald_business_unit"))

    # log_path will be NULL when function is called from webtool
    if (!is.null(log_path)) {
      paste_write(
        format_indent_1(), "Removed", n_rows_before_removal - nrow(data_with_late_and_sudden),
        "rows because negative production compensation targets were set in late and sudden production paths ways. Negative absolute production is impossible \n",
        log_path = log_path
      )
    }

    if (nrow(data_with_late_and_sudden) == 0) {
      stop("No rows remain after removing negative late and sudden trajectories.")
    }
  }

  return(data_with_late_and_sudden)
}

#' Defines which scenario values to use for the production trajectory after a
#' litigation event in the LRISK stress test.
#'
#' @description
#' Picks the corresponding values from the original scenario
#' column indicated in the input and has the option to include PACTA based
#' production forecast for the first few years of the late & sudden
#' trajectory. Similarly, it is possible to define another input scenario
#' in case the company is already aligned after the production forecast.
#' If the production forecast is included, the trajectory after the end of
#' the production forecast is offset by the initial production forecast
#' so that the remainder of the late & sudden trajectory now is a parallel
#' shift of the original scenario values. If not included, the trajectories
#' replicate externally provided scenario trajectories at least until the
#' year of the policy shock.
#' Trajectories are calculated for each company by sector, scenario_geography,
#' ald_business_unit, year.
#' If no "company_id" or "company_name" are provided, the calculation switches to
#' portfolio/ald_business_unit level.
#' @inheritParams report_company_drops
#' @param data A dataframe that contains the scenario data prepared until the
#'   step after the baseline trajectories are calculated.
#' @param litigation_scenario Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   ald_business_unit trajectories post litigation event.
#' @param shock_scenario A dataframe that contains information about the
#'   transition scenario, specifically the shock year and, duration of the
#'   shock and the name of the shock scenario
#' @param litigation_scenario_aligned Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   ald_business_unit trajectories post litigation event in case the company is
#'   aligned after the forecast period.
#' @param start_year Numeric. A numeric vector of length 1 that contains the
#'   start year of the analysis.
#' @param end_year Numeric. A numeric vector of length 1 that contains the
#'   end year of the analysis.
#' @param analysis_time_frame Numeric. A vector of length 1 indicating the number
#'   of years for which forward looking production data is considered.
#'
#' @return data frame
set_litigation_trajectory <- function(data,
                                      litigation_scenario,
                                      shock_scenario,
                                      litigation_scenario_aligned,
                                      start_year,
                                      end_year,
                                      analysis_time_frame,
                                      log_path) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography",
      "plan_tech_prod", "emission_factor", "baseline",
      litigation_scenario, litigation_scenario_aligned
    )
  )

  validate_data_has_expected_cols(
    data = shock_scenario,
    expected_columns = c(
      "year_of_shock", "scenario_name"
    )
  )

  scenario_name <- shock_scenario$scenario_name
  year_of_shock <- shock_scenario$year_of_shock

  # In LRISK, companies are forced to follow the scenario targets post
  # litigation event. Hence no compensation mechanism is built in. A potential
  # previous overshoot is compensated by paying the litigation cost.
  # We also currently only penalize companies for breaching the carbon budget on
  # declining types of capital stock or technologies. They are not sued for not
  # building out increasing technologies fast enough.
  # Emissions factors do not need to be adjusted, as they are assumed constant
  # per ald_business_unit so that the change in overall emissions is driven by changes
  # in production levels for all sectors with production pathways.
  data <- data %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      scenario_name = shock_scenario$scenario_name,
      scen_to_follow = !!rlang::sym(litigation_scenario),
      scen_to_follow_aligned = !!rlang::sym(litigation_scenario_aligned),
      scen_to_follow_change = .data$scen_to_follow - dplyr::lag(.data$scen_to_follow),
      scen_to_follow_aligned_change = .data$scen_to_follow_aligned - dplyr::lag(.data$scen_to_follow_aligned),
      baseline_scenario_change = .data$baseline - dplyr::lag(.data$baseline),
      late_sudden = .data$plan_tech_prod
    ) %>%
    dplyr::ungroup()

  reference <- data %>%
    dplyr::filter(.data$year == .env$start_year + .env$analysis_time_frame) %>%
    dplyr::select(
      dplyr::all_of(c(
        "company_id", "company_name", "ald_sector", "ald_business_unit",
        "scenario_geography", "plan_tech_prod"
      ))
    ) %>%
    dplyr::rename(
      reference_tech_prod = "plan_tech_prod"
    )

  data <- data %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      aligned = dplyr::if_else(
        .data$direction == "declining" &
          .data$late_sudden[.env$analysis_time_frame] <= .data$scen_to_follow[.env$analysis_time_frame] &
          sum(.data$late_sudden[1:.env$analysis_time_frame], na.rm = TRUE) <=
            sum(.data$scen_to_follow[1:.env$analysis_time_frame], na.rm = TRUE) |
          .data$direction == "increasing" &
            .data$late_sudden[.env$analysis_time_frame] >= .data$scen_to_follow[.env$analysis_time_frame] &
            sum(.data$late_sudden[1:.env$analysis_time_frame], na.rm = TRUE) >=
              sum(.data$scen_to_follow[1:.env$analysis_time_frame], na.rm = TRUE),
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::inner_join(
      reference,
      by = c("company_id", "company_name", "ald_sector", "ald_business_unit", "scenario_geography")
    )

  data_extended <- data %>%
    dplyr::filter(.data$year > .env$start_year + .env$analysis_time_frame) %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      late_sudden = dplyr::if_else(
        .data$aligned,
        .data$reference_tech_prod + cumsum(.data$scen_to_follow_aligned_change),
        .data$reference_tech_prod + cumsum(.data$baseline_scenario_change)
      )
    ) %>%
    dplyr::ungroup()

  data_forecast <- data %>%
    dplyr::filter(.data$year <= .env$start_year + .env$analysis_time_frame)

  data <- data_forecast %>%
    dplyr::bind_rows(data_extended) %>%
    dplyr::arrange(
      .data$company_id, .data$company_name, .data$scenario_geography, .data$ald_sector,
      .data$ald_business_unit
    )

  # only adjusting the late sudden trajectory for misaligned technologies that
  # need to decline ensures that low carbon technologies that are not built out
  # sufficiently do not get a boost out of the blue by moving to the increased
  # trajectory of the target scenario.
  data <- data %>%
    dplyr::mutate(
      late_sudden = dplyr::if_else(
        !.data$aligned & .data$year > shock_scenario$year_of_shock & .data$direction == "declining",
        .data$scen_to_follow,
        .data$late_sudden
      )
    ) %>%
    dplyr::mutate(
      company_x_biz_unit_is_litigated = dplyr::if_else(
        !.data$aligned & .data$direction == "declining",
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::group_by(.data$company_id, .data$company_name) %>%
    dplyr::mutate(company_is_litigated = any(.data$company_x_biz_unit_is_litigated)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"company_x_biz_unit_is_litigated")

  data <- filter_negative_late_and_sudden(data, log_path = log_path)

  return(data)
}
