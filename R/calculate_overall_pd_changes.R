#' Calculate change in probabilities of default (PDs) of loans connected to
#' companies at hand. This is based on the equity values derived from the DCF
#' model. Said Equity values are used as different starting points for the
#' Merton model (one reflecting the business as usual baseline scenario, the
#' other reflecting the late & sudden shock scenario). The change in PDs can
#' then be used to calculate the Expected Loss due to the shock on the portfolio
#' level.
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario
#' @param end_of_analysis A numeric vector of length one that indicates until
#'   which year the analysis runs
#' @param risk_free_interest_rate A numeric vector of length one that indicates
#'   the risk free rate of interest
calculate_pd_change_overall <- function(data,
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
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$ald_sector, .data$scenario_name,
      .data$scenario_geography, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::summarise(
      equity_0_baseline = sum(.data$discounted_net_profit_baseline, na.rm = TRUE),
      equity_0_late_sudden = sum(.data$discounted_net_profit_ls, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_name,
      .data$scenario_geography, .data$id, .data$company_name, .data$ald_sector,
      .data$equity_0_baseline, .data$equity_0_late_sudden,
      .data$debt_equity_ratio, .data$volatility
    )

  data <- data %>%
    dplyr::mutate(
      debt = .data$equity_0_baseline * .data$debt_equity_ratio,
      risk_free_rate = .env$risk_free_interest_rate,
      # ADO 1943 - see nesting step below
      term = NA_integer_
    ) %>%
    dplyr::select(-.data$debt_equity_ratio)

  nesting_names <- c(colnames(data %>% dplyr::select(-term)))

  data <- data %>%
    # ADO 1943 - this remains set to 1:5 irrespective of the main input argument,
    # as we describe the overall trend of PDs, not a change in the portfolio
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(nesting_names)),
      term = 1:5
    ) %>%
    dplyr::filter(!is.na(.data$term))

  data <- keep_merton_compatible_rows(data, stage = "overall")

  results <- data %>%
    dplyr::mutate(Survival_baseline = calc_survival_probability_merton(
      L = .data$debt,
      V0 = data$equity_0_baseline + data$debt,
      sigma = data$volatility,
      r = data$risk_free_rate,
      t = data$term
    )) %>%
    dplyr::mutate(Survival_late_sudden = calc_survival_probability_merton(
      L = data$debt,
      V0 = data$equity_0_late_sudden + data$debt,
      sigma = data$volatility,
      r = data$risk_free_rate,
      t = data$term
    )) %>%
    dplyr::mutate(
      PD_baseline = 1 - .data$Survival_baseline,
      PD_late_sudden = 1 - .data$Survival_late_sudden,
      PD_change = .data$PD_late_sudden - .data$PD_baseline
    )

  return(results)
}
