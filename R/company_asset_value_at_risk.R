#' Calculate percentage value change between scenarios for equity (and
#' temporarily other asset types) on the company-technology level
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame.
#' @param shock_scenario A dataframe containing the specification of the
#'   shock scenario at hand
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value
#' @param flat_multiplier Numeric. A ratio that determines for the asset type
#'   if how strongly the DCF should propagate to value changes.
company_technology_asset_value_at_risk <- function(data,
                                        terminal_value = NULL,
                                        shock_scenario = NULL,
                                        div_netprofit_prop_coef = NULL,
                                        flat_multiplier = NULL) {
  force(data)
  terminal_value %||% stop("Must provide input for 'terminal_value'", call. = FALSE)
  shock_scenario %||% stop("Must provide input for 'shock_scenario'", call. = FALSE)
  div_netprofit_prop_coef %||% stop("Must provide input for 'div_netprofit_prop_coef'", call. = FALSE)
  flat_multiplier %||% stop("Must provide input for 'flat_multiplier'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "discounted_net_profit_ls", "discounted_net_profit_baseline"
    )
  )
  validate_data_has_expected_cols(
    data = shock_scenario,
    expected_columns = c(
      "scenario_name", "year_of_shock", "duration_of_shock"
    )
  )

  data <- data %>%
    dplyr::filter(
      .data$year >= shock_scenario$year_of_shock,
      !is.na(.data$discounted_net_profit_ls),
      !is.na(.data$discounted_net_profit_baseline)
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_geography
    ) %>%
    dplyr::summarise(
      total_disc_npv_ls = sum(.data$discounted_net_profit_ls) *
        (1 + .env$terminal_value),
      total_disc_npv_baseline = sum(.data$discounted_net_profit_baseline) *
        (1 + .env$terminal_value),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scenario_name = .env$shock_scenario$scenario_name,
      VaR_tech_company = .env$flat_multiplier * 100 * .env$div_netprofit_prop_coef *
        (.data$total_disc_npv_ls - .data$total_disc_npv_baseline) /
        .data$total_disc_npv_baseline
    ) %>%
    dplyr::select(-c(.data$total_disc_npv_ls, .data$total_disc_npv_baseline))

  data <- data %>%
    dplyr::mutate(
      duration_of_shock = .env$shock_scenario$duration_of_shock,
      year_of_shock = .env$shock_scenario$year_of_shock
    )

  return(data)
}
