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
#' @param exclusion Optional. A dataframe with two character columns,
#'   "company_name" and "technology", that lists which technologies from which
#'   companies should be set to 0 in the remainder of the analysis.
#' @param risk_free_interest_rate A numeric vector of length one that indicates
#'   the risk free rate of interest
calculate_pd_change_overall <- function(data,
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
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$id, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_name,
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
      .data$technology, .data$equity_0_baseline, .data$equity_0_late_sudden,
      .data$debt_equity_ratio, .data$volatility
    )

  data <- data %>%
    dplyr::mutate(debt = .data$equity_0_baseline * .data$debt_equity_ratio) %>%
    dplyr::select(-.data$debt_equity_ratio)

  data <- data %>%
    dplyr::mutate(
      risk_free_rate = risk_free_interest_rate,
      term = NA_integer_
    )

  nesting_names <- c(colnames(data %>% dplyr::select(-term)))

  data <- data %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(nesting_names)),
      term = 1:5
    ) %>%
    dplyr::filter(!is.na(.data$term))

  result <- create_empty_result_df_pd_changes(
    data = data,
    horizon = "overall"
  )

  for (i in seq_along(1:nrow(data))) {
    merton_baseline <- calc_survival_probabily_merton(
      L = data$debt[i],
      V0 = data$equity_0_baseline[i] + data$debt[i],
      sigma = data$volatility[i],
      r = data$risk_free_rate[i],
      t = data$term[i]
    )

    result[i, ] <- dplyr::bind_cols(data[i, ], merton_baseline)
  }

  result <- result %>% add_cols_result_df_pd_changes(horizon = "overall")

  for (i in seq_along(1:nrow(data))) {
    merton_late_sudden <- calc_survival_probabily_merton(
      L = data$debt[i],
      V0 = data$equity_0_late_sudden[i] + data$debt[i],
      sigma = data$volatility[i],
      r = data$risk_free_rate[i],
      t = data$term[i]
    )

    result[i, "Maturity"] <- merton_late_sudden$Maturity
    result[i, "Vt"] <- merton_late_sudden$Vt
    result[i, "St"] <- merton_late_sudden$St
    result[i, "Dt"] <- merton_late_sudden$Dt
    result[i, "Survival"] <- merton_late_sudden$Survival
  }

  results <- result %>%
    dplyr::rename_with(
      ~ glue::glue("{.x}_late_sudden"),
      .cols = c(Maturity, Vt, St, Dt, Survival)
    ) %>%
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
