#' Calculate percentage value change between scenarios for equity (and
#' temporarily other asset types) on the company-technology level
#'
#' @inheritParams exclude_companies
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
company_asset_value_at_risk <- function(data,
                                        terminal_value = NULL,
                                        shock_scenario = NULL,
                                        div_netprofit_prop_coef = NULL,
                                        plan_carsten = NULL,
                                        port_aum = NULL,
                                        flat_multiplier = NULL,
                                        exclusion = NULL) {
  force(data)
  terminal_value %||% stop("Must provide input for 'terminal_value'", call. = FALSE)
  shock_scenario %||% stop("Must provide input for 'shock_scenario'", call. = FALSE)
  div_netprofit_prop_coef %||% stop("Must provide input for 'div_netprofit_prop_coef'", call. = FALSE)
  plan_carsten %||% stop("Must provide input for 'plan_carsten'", call. = FALSE)
  port_aum %||% stop("Must provide input for 'port_aum'", call. = FALSE)
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

  validate_data_has_expected_cols(
    data = plan_carsten,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "plan_carsten", "plan_sec_carsten"
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

  if (!is.null(exclusion)) {
    validate_data_has_expected_cols(
      data = exclusion,
      expected_columns = c(
        "company_name", "technology"
      )
    )

    exclusion <- exclusion %>%
      dplyr::mutate(exclude = TRUE)

    data <- data %>%
      # ADO 1945: we opt for a left join and a flag rather than an anti join so
      # that we know which values were removed in the final outcome. This
      # mechanism should be revisited as part of an overhaul of the compensation.
      dplyr::left_join(
        exclusion,
        by = c("company_name", "technology")
      ) %>%
      dplyr::mutate(
        exclude = dplyr::if_else(
          is.na(.data$exclude),
          FALSE,
          .data$exclude
        )
      )

    data <- data %>%
      dplyr::mutate(
        VaR_tech_company = dplyr::if_else(
          .data$exclude == TRUE & is.nan(.data$VaR_tech_company),
          0,
          .data$VaR_tech_company
        )
      )
  } else {
    data <- data %>%
      dplyr::mutate(exclude = FALSE)
  }

  data <- data %>%
    dplyr::inner_join(
      plan_carsten,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "technology", "ald_sector", "scenario_geography"
      )
    )

  data <- data %>%
    dplyr::inner_join(
      port_aum,
      by = c("investor_name", "portfolio_name")
    )

  data <- data %>%
    dplyr::mutate(
      tech_company_exposure = .data$asset_portfolio_value * .data$plan_carsten,
      tech_company_value_change = .data$tech_company_exposure * .data$VaR_tech_company / 100
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_company = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      company_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      company_value_change = .data$company_exposure * .data$VaR_company / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$technology, .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_technology = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_value_change = .data$technology_exposure * .data$VaR_technology / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_sector = sum(.data$VaR_technology * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_value_change = .data$sector_exposure * .data$VaR_sector / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_analysed_sectors = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_value_change = .data$analysed_sectors_exposure *
        .data$VaR_analysed_sectors / 100,
      portfolio_aum = .data$asset_portfolio_value,
      # setting portfolio_value_change = analysed_sectors_value_change will
      # underestimate overall impact on portfolio as there can of course be
      # impacts on companies in the portfolio that operate in other sectors
      portfolio_value_change = .data$analysed_sectors_value_change,
      portfolio_value_change_perc = sum(.data$VaR_tech_company * .data$tech_company_exposure, na.rm = TRUE) /
        .data$portfolio_aum
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::mutate(
      duration_of_shock = .env$shock_scenario$duration_of_shock,
      year_of_shock = .env$shock_scenario$year_of_shock,
      production_shock_perc = NA_real_
    ) %>%
    dplyr::select(-c(.data$plan_carsten, .data$plan_sec_carsten, .data$year))

  return(data)
}
