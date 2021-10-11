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
#' @param exclusion Optional. A dataframe with two character columns,
#'   "company_name" and "technology", that lists which technologies from which
#'   companies should be set to 0 in the remainder of the analysis.
#' @param risk_free_interest_rate A numeric vector of length one that indicates
#'   the risk free rate of interest
calculate_pd_change_annual <- function(data,
                                       shock_year = NULL,
                                       end_of_analysis = NULL,
                                       exclusion = NULL,
                                       risk_free_interest_rate = NULL) {
  force(data)
  shock_year %||% stop("Must provide input for 'shock_year'", call. = FALSE)
  end_of_analysis %||% stop("Must provide input for 'end_of_analysis'", call. = FALSE)
  risk_free_interest_rate %||% stop("Must provide input for 'risk_free_interest_rate'", call. = FALSE)

  data_has_expected_columns <- all(
    c(
      "investor_name", "portfolio_name", "id", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "scenario_name", "discounted_net_profit_ls",
      "discounted_net_profit_baseline", "debt_equity_ratio", "volatility"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  data <- data %>%
    dplyr::filter(.data$year >= .env$shock_year) %>%
    dplyr::arrange(
      .data$scenario_name, .data$scenario_geography, .data$investor_name,
      .data$portfolio_name, .data$id, .data$company_name, .data$ald_sector,
      .data$technology, .data$year, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_name,
      .data$scenario_geography, .data$debt_equity_ratio, .data$volatility
    ) %>%
    dplyr::mutate(
      equity_t_baseline = cumsum(.data$discounted_net_profit_baseline),
      equity_t_late_sudden = cumsum(.data$discounted_net_profit_ls)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario_name,
      .data$scenario_geography, .data$id, .data$company_name, .data$ald_sector,
      .data$technology, .data$year, .data$equity_t_baseline,
      .data$equity_t_late_sudden, .data$debt_equity_ratio, .data$volatility
    )

  data <- data %>%
    dplyr::mutate(debt = .data$equity_t_baseline * .data$debt_equity_ratio) %>%
    dplyr::select(-.data$debt_equity_ratio)

  data <- data %>%
    dplyr::mutate(
      risk_free_rate = risk_free_interest_rate,
      term = 1 # annual
    )

  result <- create_empty_result_df_pd_changes(
    data = data,
    horizon = "annual"
  )

  for (i in seq_along(1:nrow(data))) {
    merton_baseline <- CreditRisk::Merton(
      L = data$debt[i],
      V0 = data$equity_t_baseline[i] + data$debt[i],
      sigma = data$volatility[i],
      r = data$risk_free_rate[i],
      t = data$term[i]
    )

    result[i, ] <- dplyr::bind_cols(data[i, ], dplyr::select(merton_baseline, Survival))
  }

  result <- result %>% add_cols_result_df_pd_changes(horizon = "annual")

  for (i in seq_along(1:nrow(data))) {
    merton_late_sudden <- CreditRisk::Merton(
      L = data$debt[i],
      V0 = data$equity_t_late_sudden[i] + data$debt[i],
      sigma = data$volatility[i],
      r = data$risk_free_rate[i],
      t = data$term[i]
    )

    result[i, "Survival"] <- merton_late_sudden$Survival
  }

  results <- result %>%
    dplyr::rename(Survival_late_sudden = Survival) %>%
    dplyr::mutate(
      PD_baseline = 1 - .data$Survival_baseline,
      PD_late_sudden = 1 - .data$Survival_late_sudden,
      PD_change = .data$PD_late_sudden - .data$PD_baseline
    )

  if (!is.null(exclusion)) {
    exclusion_has_expected_columns <- all(
      c("company_name", "technology") %in% colnames(exclusion)
    )
    stopifnot(exclusion_has_expected_columns)

    exclusion <- exclusion %>%
      dplyr::mutate(exclude = TRUE)

    results <- results %>%
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

    results <- results %>%
      dplyr::mutate(
        PD_change = dplyr::if_else(
          .data$exclude == TRUE,
          0,
          .data$PD_change
        )
      )
  } else {
    return(results)
  }
}
