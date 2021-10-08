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
#' If no "id" or "company_name" are provided, the calculation switches to
#' portfolio/technology level.
#'
#' @param data A dataframe that contains scenario trajectories by technology
#'   until 2040 for all the scenarios included in the analysis and
#'   production build out plans by technology or company and technology,
#'   usually for 5 years, based on PACTA results.
#' @param scenario_to_follow_baseline Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   baseline technology trajectories.
#' @param use_prod_forecasts Logical. Indicates whether or not to offset the
#'   baseline trajectories with the actual production plans for the first few
#'   years
#'
#' @family scenario definition
#'
#' @return dataframe
#'
#' @export
set_baseline_trajectory <- function(data,
                                    scenario_to_follow_baseline = "NPS",
                                    use_prod_forecasts = TRUE) {
  if (!"id" %in% names(data)) {
    data$id <- "PortfolioLevel"
  }
  if (!"company_name" %in% names(data)) {
    data$company_name <- "PortfolioLevel"
  }

  data_has_expected_columns <- all(c(
    "investor_name", "portfolio_name", "id", "company_name",
    "ald_sector", "technology", "scenario_geography",
    "plan_tech_prod", scenario_to_follow_baseline
  ) %in% colnames(data))
  stopifnot(data_has_expected_columns)

  data <- data %>%
    dplyr::group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$id,
      .data$company_name,
      .data$ald_sector,
      .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      scen_to_follow = !!rlang::sym(scenario_to_follow_baseline)
    ) %>%
    dplyr::mutate(
      scenario_change = .data$scen_to_follow - dplyr::lag(.data$scen_to_follow),
      baseline = dplyr::if_else(
        rep(use_prod_forecasts, dplyr::n()),
        calc_future_prod_follows_scen(
          planned_prod = .data$plan_tech_prod,
          change_scen_prod = .data$scenario_change
        ),
        .data$scen_to_follow
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$scenario_change, .data$scen_to_follow))
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
#'   company or (aggregated) technology to be included. The length of the vector
#'   for each company is from the start year of the analysis to the end year of
#'   the analysis, which means that in most cases, this vector will include NAs
#'   after the final forecast year. This usually comes from a PACTA analysis.
#' @param change_scen_prod Numeric vector that contains the absolute changes of
#'   the production trajectories according to the baseline scenario. After the
#'   end of the production forecast period (i.e. the first NA in planned_prod),
#'   the planned production is extended until the end of the analysis period
#'   using these absolute changes. This results in an offset that is a parallell
#'   shift relative to the original baseline scenario.
#'
#' @family scenario definition
#'
#' @return numeric vector
#'
#' @export
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
#' technology, year.
#' If no "id" or "company_name" are provided, the calculation switches to
#' portfolio/technology level.
#'
#' @param data A dataframe that contains the scenario data prepared until the
#'   step after the baseline trajectories are calculated.
#' @param scenario_to_follow_ls Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden technology trajectories.
#' @param shock_scenario A dataframe that contains information about the
#'   transition scenario, specifically the shock year, the overshoot method,
#'   whether to include production forecasts and potentially explicit shock
#'   sizes.
#' @param use_production_forecasts_ls Logical. A logical vector of length 1
#'   that indicates whether or not the late & sudden trajectory should make
#'   use of the production forecast provided by PACTA results.
#' @param overshoot_method Logical. A logical vector of length 1 that indicates
#'   if (when TRUE) the integral/overshoot method should be used to calculate
#'   shock size endogenously based on carbon budgets, or (when FALSE) the shock
#'   sizes need to be supplied by the user.
#' @param scenario_to_follow_ls_aligned Character. A string that indicates which
#'   of the scenarios included in the analysis should be used to set the
#'   late & sudden technology trajectories in case the company is aligned after
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
#'
#' @export
set_ls_trajectory <- function(data,
                              scenario_to_follow_ls = "SDS",
                              shock_scenario = shock_scenario,
                              use_production_forecasts_ls = TRUE,
                              overshoot_method = TRUE,
                              scenario_to_follow_ls_aligned = "SDS",
                              start_year = 2020,
                              end_year = 2040,
                              analysis_time_frame = NULL) {
  analysis_time_frame %||% stop("Must provide input for 'time_frame'", call. = FALSE)

  year_of_shock <- shock_scenario$year_of_shock
  duration_of_shock <- shock_scenario$duration_of_shock

  if (!"id" %in% names(data)) {
    data$id <- "PortfolioLevel"
  }
  if (!"company_name" %in% names(data)) {
    data$company_name <- "PortfolioLevel"
  }

  data_has_expected_columns <- all(c(
    "investor_name", "portfolio_name", "id", "company_name",
    "ald_sector", "technology", "scenario_geography",
    "plan_tech_prod", "baseline",
    scenario_to_follow_ls, scenario_to_follow_ls_aligned
  ) %in% colnames(data))
  stopifnot(data_has_expected_columns)

  shock_scenario_has_expected_columns <- all(c(
    "year_of_shock", "duration_of_shock", "scenario_name"
  ) %in% colnames(shock_scenario))
  stopifnot(shock_scenario_has_expected_columns)

  shock_scenario <- shock_scenario %>%
    dplyr::select(-c(.data$year_of_shock, .data$duration_of_shock)) %>%
    tidyr::pivot_longer(
      cols = -c("scenario_name"),
      names_to = "technology",
      values_to = "shock_strength"
    )

  data <- data %>%
    dplyr::left_join(
      shock_scenario,
      by = "technology"
    ) %>%
    dplyr::mutate(scen_to_follow = !!rlang::sym(scenario_to_follow_ls)) %>%
    dplyr::mutate(scen_to_follow_aligned = !!rlang::sym(scenario_to_follow_ls_aligned)) %>%
    dplyr::mutate(
      late_sudden = dplyr::if_else(
        rep(use_production_forecasts_ls, dplyr::n()),
        .data$plan_tech_prod,
        .data$baseline
      )
    )

  data <- data %>%
    dplyr::group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$id,
      .data$company_name,
      .data$ald_sector,
      .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      scenario_change = .data$scen_to_follow - dplyr::lag(.data$scen_to_follow),
      scenario_change_aligned = .data$scen_to_follow_aligned - dplyr::lag(.data$scen_to_follow_aligned),
      scenario_change_baseline = .data$baseline - dplyr::lag(.data$baseline),
      overshoot_direction = rep(
        dplyr::if_else(
          .data$scen_to_follow[1] - .data$scen_to_follow[length(.data$scen_to_follow)] > 0,
          "Decreasing",
          "Increasing"
        ),
        dplyr::n()
      )
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::group_by(
      .data$investor_name,
      .data$portfolio_name,
      .data$id,
      .data$company_name,
      .data$ald_sector,
      .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      late_sudden = dplyr::if_else(
        shock_strength == 0 & !overshoot_method,
        .data$baseline,
        calc_late_sudden_traj(
          start_year = start_year,
          end_year = end_year,
          year_of_shock = year_of_shock,
          duration_of_shock = duration_of_shock,
          shock_strength = .data$shock_strength,
          scen_to_follow = .data$scen_to_follow,
          planned_prod = .data$plan_tech_prod,
          late_and_sudden = .data$late_sudden,
          scenario_change = .data$scenario_change,
          scenario_change_baseline = .data$scenario_change_baseline,
          scenario_change_aligned = .data$scenario_change_aligned,
          overshoot_method = overshoot_method,
          overshoot_direction = .data$overshoot_direction[1],
          time_frame = .env$analysis_time_frame
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c(
        .data$scen_to_follow,
        .data$shock_strength,
        .data$scenario_change,
        .data$scenario_change_baseline,
        .data$overshoot_direction
      )
    )

  data <- filter_negative_late_and_sudden(data)

  return(data)
}


#' Calculate how the production trajectory for a company/technology changes
#' after the policy shock hits.
#'
#' @description
#' Prior to the shock, this will keep the
#' trajectory untouched, i.e., the trajectory follows baseline up until the
#' shock. After the shock hits, the development depends on whether or not
#' the company/technology is already aligned and on what type of calculation
#' is selected for the shock. Overall the outcome should lead the
#' company/technology to stay within the bounds of the carbon budget, if the
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
#'   size for the given company/technology at hand, in case shock size is not
#'   calculated endogenously by using the overshoot/carbon budget method.
#'   TODO: (move to data argument)
#' @param scen_to_follow Numeric. A numeric vector that contains the production
#'   trajectory of the scenario indicated to use as the target for the
#'   company/technology at hand.
#'   TODO: (move to data argument)
#' @param planned_prod Numeric vector that includes the production plans for a
#'   company or (aggregated) technology to be included. The length of the vector
#'   for each company is from the start year of the analysis to the end year of
#'   the analysis, which means that in most cases, this vector will include NAs
#'   after the final forecast year. This usually comes from a PACTA analysis.
#'   TODO: (move to data argument)
#' @param late_and_sudden Numeric. A numeric vector that contains the
#'   late & sudden production trajectory for the company/technology at hand.
#'   Before applying the shock, this follows the baseline scenario.
#'   TODO: (move to data argument)
#' @param scenario_change Numeric. A numeric vector that contains the
#'   absolute changes of the target scenario in yearly steps for the
#'   company/technology at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_baseline Numeric. A numeric vector that contains the
#'   absolute changes of the baseline scenario in yearly steps for the
#'   company/technology at hand.
#'   TODO: (move to data argument)
#' @param scenario_change_aligned Numeric. A numeric vector that contains the
#'   absolute changes of the aligned target scenario in yearly steps for the
#'   company/technology at hand, in case the company/technology is aligned
#'   with the target after the forecast period.
#'   TODO: (move to data argument)
#' @param overshoot_method Logical. A logical vector of length 1 that indicates
#'   if (when TRUE) the integral/overshoot method should be used to calculate
#'   shock size endogenously based on carbon budgets, or (when FALSE) the shock
#'   sizes need to be supplied by the user.
#' @param overshoot_direction Character. A character vector that indicates if
#'   the technology at hand is increasing or decreasing over the time frame of
#'   the analysis.
#'   TODO: (move to data argument)
#' @param time_frame Numeric. A vector of length 1 indicating the number of years
#'   for which forward looking production data is considered.
#'
#' @family scenario definition
#'
#' @return numeric vector
#'
#' @export
calc_late_sudden_traj <- function(start_year, end_year, year_of_shock, duration_of_shock,
                                  shock_strength, scen_to_follow, planned_prod, late_and_sudden,
                                  scenario_change, scenario_change_baseline, scenario_change_aligned,
                                  overshoot_method, overshoot_direction, time_frame) {
  time_frame %||% stop("Must provide input for 'time_frame'", call. = FALSE)

  # calculate the position where the shock kicks in
  position_shock_year <- year_of_shock - start_year + 1

  # first position for which future production is unknown
  first_production_na <- which(is.na(planned_prod))[1]
  late_and_sudden[first_production_na:length(late_and_sudden)] <- NA

  if (!is.na(which(is.na(late_and_sudden[1:position_shock_year]))[1])) {
    # if this is true, then there are NA's in the period after the company prod
    # forecasts and the shock period
    # i.e. we need to fill the values between the last year we have production
    # forecasts, and the year of the shock
    # for example, if the shock year is 2026 and we have production forecasts
    # until 2024, we need to calculate L&S production for 2025 (we follow
    # baseline as it is the late & sudden scen)

    for (i in which(is.na(late_and_sudden[1:position_shock_year]))[1]:position_shock_year) {
      late_and_sudden[i] <- late_and_sudden[i - 1] + scenario_change_baseline[i]
    }
  }

  # integral/overshoot compensation method
  if (overshoot_method) {

    # If the company production plans are already aligned (or outperforming SDS),
    # we do not need to compensate production capacity, and we set
    # the LS trajectory equal to the SDS trajectory
    if ((
      overshoot_direction == "Decreasing" &
        sum(scen_to_follow[1:time_frame]) < sum(late_and_sudden[1:time_frame])
    ) | (
      overshoot_direction == "Increasing" &
        sum(scen_to_follow[1:time_frame]) > sum(late_and_sudden[1:time_frame])
    )
    ) {
      x <- (sum(scen_to_follow) -
        sum(late_and_sudden[1:(position_shock_year - 1)]) -
        (end_year - year_of_shock + 1) * late_and_sudden[position_shock_year - 1]) /
        (-sum(seq(1, end_year - year_of_shock + 1)))

      # add the absolute production increase/decrease for each year during
      # the shock period, capping at a 0 lower bound for production volume
      for (j in seq(position_shock_year, length(scen_to_follow))) {
        late_and_sudden[j] <- max(
          late_and_sudden[position_shock_year - 1] - (j - position_shock_year + 1) * x,
          0
        )
      }

      shock_strength <- 100 *
        (late_and_sudden[position_shock_year + 1] - late_and_sudden[position_shock_year]) /
        late_and_sudden[position_shock_year]
    } else {
      # company plans are already aligned
      # no need for overshoot in production cap, set LS trajectory to follow
      # the scenario indicated as late & sudden aligned

      for (k in seq(first_production_na, length(scen_to_follow))) {
        late_and_sudden[k] <- late_and_sudden[k - 1] + scenario_change_aligned[k]
      }
    }
  } else {
    # Calculates the production increase/decrease by taking the scenario value
    # at the year of shock and multiplying it by the corresponding shock strength.
    # We apply the increase/decrease calculated in abs_prod_shock
    # for each year in the shock period.
    abs_prod_shock <- late_and_sudden[position_shock_year] *
      (shock_strength[1] / 100)

    # add the absolute production increase/decrease for each year during
    # the shock period, capping at a 0 lower bound for production volume
    for (j in seq(0, duration_of_shock - 1)) {
      late_and_sudden[position_shock_year + j] <- max(
        late_and_sudden[position_shock_year + j - 1] + abs_prod_shock,
        0
      )
    }
    # after the shock period, let the late & sudden scenario follow
    # the scenario trajectory set with variable 'scenario_to_follow_ls'
    for (k in seq(duration_of_shock + position_shock_year, length(scen_to_follow))) {
      late_and_sudden[k] <- late_and_sudden[k - 1] + scenario_change[k]
    }
  }

  return(late_and_sudden)
}

#' Remove negative late and sudden rows
#'
#' Function checks for negative values on variable late_and_sudden. All
#' technology x company_name combinations holding >= 1 negative value are
#' removed.
#'
#' @param data_with_late_and_sudden A tibble containing scenario data with
#'   projected late and sudden trajectory.
#'
#' @return Input tibble with potentially removed rows.
filter_negative_late_and_sudden <- function(data_with_late_and_sudden) {
  negative_late_and_sudden <- data_with_late_and_sudden %>%
    dplyr::filter(.data$late_and_sudden < 0) %>%
    dplyr::select(.data$company_name, .data$technology) %>%
    dplyr::distinct()

  if (nrow(negative_late_and_sudden) > 0) {
    n_rows_before_removal <- nrow(data_with_late_and_sudden)

    data_with_late_and_sudden <-
      data_with_late_and_sudden %>%
      dplyr::anti_join(negative_late_and_sudden, by = c("company_name", "technology"))

    warning(paste0("Removed ", n_rows_before_removal - nrow(data_with_late_and_sudden),
                   " rows due to negative late and sudden targets."))

    if (nrow(data_with_late_and_sudden) == 0) {
      stop("No rows remain after removing negative late and sudden trajectories.")
    }
  }

  return(data_with_late_and_sudden)
}

