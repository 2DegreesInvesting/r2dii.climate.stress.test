#' Function that renames Merton model baseline result columns and adds additional
#' columns for the second application of the model in the late & sudden scenario.
#'
#' @param data A dataframe that contains the result data from the baseline
#'   Merton model
#' @param horizon A cahracter vector of length 1 indicating whether to return
#'   a result obejct for overall or yearly results. Must be "annual" or "overall"
#'
#' @family helper functions
#'
#' @export

add_cols_result_df_pd_changes <- function(data,
                                          horizon = NULL) {
  force(data)
  horizon %||% stop("Must provide input for 'horizon'", call. = FALSE)

  horizon_allowed <- horizon %in% c("annual", "overall")
  stopifnot(horizon_allowed)

  common_columns <- c(
    "investor_name", "portfolio_name", "scenario_name", "scenario_geography",
    "id", "company_name", "ald_sector", "technology", "debt", "volatility",
    "risk_free_rate", "term","Survival"
  )

  expected_columns <- c(
    common_columns, "year", "equity_t_baseline", "equity_t_late_sudden"
  )

  if (identical(horizon, "overall")) {
    expected_columns <- c(
      common_columns, "equity_0_baseline", "equity_0_late_sudden"
    )
  }

  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)

  data <- data %>%
    dplyr::rename_with(
      ~ glue::glue("{.x}_baseline"),
      .cols = c(.data$Survival)
    ) %>%
    dplyr::mutate(
      Survival = NA_real_
    )

  data
}
