#' Calculate percentage value change between scenarios for equity (and
#' temporarily other asset types) on the portfolio level
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame.
#' @param shock_scenario A dataframe containing the specification of the
#'   shock scenario at hand
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value
#' @param plan_carsten A dataframe that contains the share of the portfolio
#'   value of each company-technology combination. Used to quantify the impact of
#'   the company-tech level shock on higher levels of aggregation in the pf
#' @param port_aum A dataframe containing the value of the portfolio for
#'   the asset type at hand
#' @param flat_multiplier Numeric. A ratio that determines for the asset type
#'   if how strongly the DCF should propagate to value changes.

asset_value_at_risk <- function(data,
                                terminal_value = NULL,
                                shock_scenario = NULL,
                                div_netprofit_prop_coef = NULL,
                                plan_carsten = NULL,
                                port_aum = NULL,
                                flat_multiplier = NULL) {
  force(data)
  terminal_value %||% stop("Must provide input for 'terminal_value'", call. = FALSE)
  shock_scenario %||% stop("Must provide input for 'shock_scenario'", call. = FALSE)
  div_netprofit_prop_coef %||% stop("Must provide input for 'div_netprofit_prop_coef'", call. = FALSE)
  plan_carsten %||% stop("Must provide input for 'plan_carsten'", call. = FALSE)
  port_aum %||% stop("Must provide input for 'port_aum'", call. = FALSE)
  flat_multiplier %||% stop("Must provide input for 'flat_multiplier'", call. = FALSE)

  data_has_expected_columns <- all(
    c(
      "investor_name", "portfolio_name", "year",
      "scenario_geography", "ald_sector", "technology",
      # "scenario_name",
      "discounted_net_profit_ls", "discounted_net_profit_baseline"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  shock_scenario_has_expected_columns <- all(
    c(
      "scenario_name", "year_of_shock", "duration_of_shock"
    ) %in% colnames(shock_scenario)
  )
  stopifnot(shock_scenario_has_expected_columns)

  plan_carsten_has_expected_columns <- all(
    c(
      "investor_name", "portfolio_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "plan_carsten", "plan_sec_carsten"
    ) %in% colnames(plan_carsten)
  )
  stopifnot(plan_carsten_has_expected_columns)

  data <- data %>%
    dplyr::filter(
      .data$year >= shock_scenario$year_of_shock,
      !is.na(.data$discounted_net_profit_ls),
      !is.na(.data$discounted_net_profit_baseline)
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$technology, .data$scenario_geography
    ) %>%
    dplyr::summarise(
      total_disc_npv_ls = sum(.data$discounted_net_profit_ls, na.rm = TRUE) *
        (1 + .env$terminal_value),
      total_disc_npv_baseline = sum(.data$discounted_net_profit_baseline, na.rm = TRUE) *
        (1 + .env$terminal_value),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      VaR_technology = .env$flat_multiplier * 100 * .env$div_netprofit_prop_coef *
        (.data$total_disc_npv_ls - .data$total_disc_npv_baseline) /
        .data$total_disc_npv_baseline
    ) %>%
    dplyr::select(-c(.data$total_disc_npv_ls, .data$total_disc_npv_baseline))

  data <- data %>%
    dplyr::inner_join(
      plan_carsten,
      by = c("investor_name", "portfolio_name", "technology", "ald_sector", "scenario_geography")
    )

  data <- data %>%
    dplyr::inner_join(
      port_aum,
      by = c("investor_name", "portfolio_name")
    )

  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_sector = sum(.data$VaR_technology * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      scenario_name = .env$shock_scenario$scenario_name,
      technology_exposure = .data$asset_portfolio_value * .data$plan_carsten,
      sector_exposure = .data$asset_portfolio_value * .data$plan_sec_carsten,
      sector_loss = .data$sector_exposure * .data$VaR_sector / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography
    ) %>%
    dplyr::mutate(
      climate_relevant_var = sum(.data$VaR_technology * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      portfolio_aum = .data$asset_portfolio_value,
      portfolio_loss_perc = sum(.data$VaR_technology * .data$technology_exposure, na.rm = TRUE) /
        .data$asset_portfolio_value
    ) %>%
    dplyr::ungroup()

  shock_scenario_long <- shock_scenario %>%
    tidyr::pivot_longer(
      cols = -c(scenario_name, year_of_shock, duration_of_shock),
      names_to = "technology",
      values_to = "production_shock_perc"
    )

  data <- data %>%
    dplyr::inner_join(
      shock_scenario_long,
      by = c("scenario_name", "technology")
    ) %>%
    dplyr::select(-c(.data$plan_carsten, .data$plan_sec_carsten, .data$year))

  return(data)
}
