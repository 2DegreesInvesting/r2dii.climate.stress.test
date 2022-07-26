#' Calculate change in probabilities of default (PDs) of loans connected to
#' companies at hand. This is based on the equity values derived from the DCF
#' model. Said Equity values are used as different starting points for the
#' Merton model (one reflecting the business as usual baseline scenario, the
#' other reflecting the late & sudden shock scenario). Since we calculate the PDs
#' on an annual basis (start year up to year t), we use different NPVs based on
#' the DCF model for every year that we calculate a PD for. Only discounted
#' profits up until the year in question will be taken into account as the equity
#' starting points for each of the calculations. Prior to a policy shock, this
#' implies equal starting points for baseline and late and sudden scenarios and
#' only after the shock, we get diverging starting points.
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario
#' @param end_of_analysis A numeric vector of length one that indicates until
#'   which year the analysis runs
#' @param risk_free_interest_rate A numeric vector of length one that indicates
#'   the risk free rate of interest
calculate_pd_change_annual <- function(data,
                                       shock_year = NULL,
                                       end_of_analysis = NULL,
                                       risk_free_interest_rate = NULL) {
  force(data)
  shock_year %||% stop("Must provide input for 'shock_year'", call. = FALSE)
  end_of_analysis %||% stop("Must provide input for 'end_of_analysis'", call. = FALSE)
  risk_free_interest_rate %||% stop("Must provide input for 'risk_free_interest_rate'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "id", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "scenario_name", "discounted_net_profit_ls",
      "discounted_net_profit_baseline", "debt_equity_ratio", "volatility"
    )
  )

  data <- data %>%
    dplyr::filter(.data$year >= .env$shock_year) %>%
    # ADO 2313 - summarise by company, drop technology
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$id, .data$company_name, .data$ald_sector,
      .data$year, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::summarise(
      discounted_net_profit_baseline = sum(.data$discounted_net_profit_baseline, na.rm = TRUE),
      discounted_net_profit_ls = sum(.data$discounted_net_profit_ls, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    # ADO 2313 -  cumulative sum by year to obtain inputs for annual PD calculations
    dplyr::arrange(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$id, .data$company_name, .data$ald_sector,
      .data$year, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::group_by(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$id, .data$company_name, .data$ald_sector,
      # TODO: clarify the appropriate summation for annual PD calculations
      # .data$debt_equity_ratio, .data$volatility
      .data$year, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::mutate(
      equity_t_baseline = cumsum(.data$discounted_net_profit_baseline),
      equity_t_late_sudden = cumsum(.data$discounted_net_profit_ls)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_name,
      .data$scenario_geography, .data$id, .data$company_name, .data$ald_sector,
      .data$year, .data$equity_t_baseline, .data$equity_t_late_sudden,
      .data$debt_equity_ratio, .data$volatility
    )

  data <- data %>%
    dplyr::mutate(
      debt = .data$equity_t_baseline * .data$debt_equity_ratio,
      risk_free_rate = .env$risk_free_interest_rate,
      # ADO 1943 - this remains set to 1 irrespective of the main input argument,
      # as we describe the overall annual trend, not a change in the portfolio
      term = 1
    ) %>%
    dplyr::select(-.data$debt_equity_ratio)

  data <- keep_merton_compatible_rows(data, stage = "annual")

  results <- data %>%
    dplyr::mutate(Survival_baseline = calc_survival_probability_merton(
      L = .data$debt,
      V0 = .data$equity_t_baseline + .data$debt,
      sigma = .data$volatility,
      r = .data$risk_free_rate,
      t = .data$term
    )) %>%
    dplyr::mutate(Survival_late_sudden = calc_survival_probability_merton(
      L = .data$debt,
      V0 = .data$equity_t_late_sudden + .data$debt,
      sigma = .data$volatility,
      r = .data$risk_free_rate,
      t = .data$term
    )) %>%
    dplyr::mutate(
      PD_baseline = 1 - .data$Survival_baseline,
      PD_late_sudden = 1 - .data$Survival_late_sudden,
      PD_change = .data$PD_late_sudden - .data$PD_baseline
    )

  return(results)
}
