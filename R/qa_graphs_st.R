#' Display the market price trajectories per technology
#'
#' @param data A dataframe that contains market prices for a list of scenarios.
#' @param price_scenarios Character. A vector containing the names of the
#'   scenarios to display in the chart.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_price_trajectories <- function(data = df_price,
                                    price_scenarios = c(
                                      "B2DS", "NPS", "SDS", "Baseline"
                                    )) {
  data_has_expected_columns <- all(
    c("year", "sector", "technology", price_scenarios) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  price_long <- data %>%
    dplyr::select(c(
      "year", "sector", "technology",
      dplyr::all_of(price_scenarios)
    )) %>%
    tidyr::pivot_longer(
      cols = c(!!!rlang::syms(price_scenarios)),
      names_to = "scenario",
      values_to = "price"
    )

  price_trajectories <- price_long %>%
    ggplot(aes(x = .data$year, y = .data$price, color = .data$scenario)) +
    geom_line(alpha = .5) +
    expand_limits(y = 0) +
    facet_wrap(vars(.data$technology), scales = "free")
}

#' Display the market production trajectories per technology
#'
#' @param data A dataframe that contains scenario data.
#' @param end_year Numeric. Final year of the analysis/trajectories.
#' @param source Character. A vector of allowed scenario sources.
#' @param ald_sector Character. A vector of allowed sectors.
#' @param technology Character. A vector of allowed technologies.
#' @param geography_filter Character. A vector of included scenario geographies.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_prod_trajectories <- function(data = scenario_data,
                                   end_year = 2040,
                                   source = NULL,
                                   ald_sector = NULL,
                                   technology = NULL,
                                   geography_filter = NULL) {
  data_has_expected_columns <- all(c(
    "source", "year", "ald_sector",
    "technology", "scenario_geography",
    "scenario", "fair_share_perc"
  )
  %in% colnames(data))
  stopifnot(data_has_expected_columns)

  production_over_time <- data %>%
    dplyr::filter(
      .data$year <= end_year &
        .data$source %in% source &
        .data$ald_sector %in% ald_sector &
        .data$technology %in% technology &
        .data$scenario_geography %in% geography_filter
    ) %>%
    ggplot(
      aes(x = .data$year, y = .data$fair_share_perc, color = .data$scenario)
    ) +
    geom_line(alpha = .5) +
    expand_limits(y = 0) +
    facet_wrap(
      vars(.data$technology, .data$scenario_geography),
      scales = "free"
    )
}



#' Display impact on value at risk by technology and shock year
#'
#' @param data A dataframe that contains stress test results for one asset type.
#' @param level Character. A string, either "technology" or "ald_sector",
#'   indicating the desired level of aggregation.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_impact_by_shock_year <- function(data,
                                      level = NULL) {
  valid_level_input <- level %in% c("technology", "ald_sector")
  stopifnot(valid_level_input)

  if (level == "technology") {
    var_level <- "VaR_technology"
  } else {
    var_level <- "VaR_sector"
  }

  data_has_expected_columns <- all(c(
    "year_of_shock", level, var_level
  ) %in% colnames(data))
  stopifnot(data_has_expected_columns)


  percentage_value_change_by_shock_year <- data %>%
    ggplot(
      aes(
        x = factor(!!rlang::sym(level)),
        y = !!rlang::sym(var_level),
        color = !!rlang::sym(var_level) > 0,
        size = .data$year_of_shock,
        group = .data$year_of_shock
      )
    ) +
    geom_point(aes(alpha = 0.5)) +
    expand_limits(y = 0) +
    geom_hline(yintercept = 0) +
    theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
}

#' Display change of results by technology and shock year
#'
#' @param data A dataframe that contains stress test results for one asset type.
#' @param level Character. A string, either "technology" or "ald_sector",
#'   indicating the desired level of aggregation.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_var_change_by_shock_year <- function(data,
                                          level = NULL) {
  valid_level_input <- level %in% c("technology", "ald_sector")
  stopifnot(valid_level_input)

  if (level == "technology") {
    var_level <- "VaR_technology"
  } else {
    var_level <- "VaR_sector"
  }

  data_has_expected_columns <- all(c(
    "year_of_shock", level, var_level
  ) %in% colnames(data))
  stopifnot(data_has_expected_columns)


  percentage_value_change_by_shock_year <- data %>%
    ggplot(
      aes(
        x = .data$year_of_shock,
        y = !!rlang::sym(var_level),
        color = !!rlang::sym(var_level) > 0
      )
    ) +
    geom_col(position = "dodge") +
    expand_limits(y = 0) +
    facet_wrap(
      vars(!!rlang::sym(level)),
      scales = "free"
    ) +
    theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )
}


#' Compare the production trajectories per technology by baseline, target and
#' late_and_sudden scenarios
#'
#' @param data A dataframe that contains the annual_profits output.
#' @param geography_filter Character. A vector of included scenario geographies.
#' @param shock_year Numeric. Indicates the year of shock for which to display
#'   the production trajectories.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_prod_baseline_target_ls_pf <- function(data,
                                            geography_filter = NULL,
                                            shock_year = NULL) {
  data_has_expected_columns <- all(c(
    "scenario_geography", "year_of_shock",
    "baseline", "scen_to_follow_aligned",
    "late_sudden"
  ) %in% colnames(data))
  stopifnot(data_has_expected_columns)

  data_long <- data %>%
    dplyr::filter(
      .data$scenario_geography %in% geography_filter &
        .data$year_of_shock == shock_year
      # investor_name, porfolio_name, company_name???
    ) %>%
    tidyr::pivot_longer(
      cols = c(.data$baseline, .data$scen_to_follow_aligned, .data$late_sudden),
      names_to = "metric",
      values_to = "production"
    )

  baseline_target_ls_pf <- data_long %>%
    ggplot(aes(x = .data$year, y = .data$production, color = .data$metric)) +
    geom_line(alpha = .5) +
    expand_limits(y = 0) +
    facet_wrap(
      vars(.data$technology, .data$scenario_geography),
      scales = "free"
    )
}

#' Check the technology shares
#'
#' @param data A dataframe that contains the technology share of the portfolio
#'   for one of the asset types.
#'
#' @family qa graphing functions
#'
#' @return ggplot object
#'
#' @export
show_pf_technology_shares <- function(data) {
  data_has_expected_columns <- all(c("ald_sector", "technology", "plan_carsten")
  %in% colnames(data))
  stopifnot(data_has_expected_columns)

  tech_share <- data %>%
    ggplot(
      aes(x = .data$ald_sector, y = .data$plan_carsten, fill = .data$technology)
    ) +
    geom_col(color = "#000000") +
    expand_limits(y = 0)
}
