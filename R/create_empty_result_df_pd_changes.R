#' Function that creates an empty tibble/data.frame for results of the Merton
#' model.
#'
#' @param data A dataframe that contains the input data to the Merton model
#' @param horizon A cahracter vector of length 1 indicating whether to return
#'   a result obejct for overall or yearly results. Must be "annual" or "overall"
#'
#' @family helper functions
#'
#' @export

create_empty_result_df_pd_changes <- function(data,
                                              horizon = NULL) {
  force(data)
  horizon %||% stop("Must provide input for 'horizon'", call. = FALSE)

  horizon_allowed <- horizon %in% c(
    "annual",
    "overall"
  )
  stopifnot(horizon_allowed)

  if (horizon == "overall") {
    data_has_expected_columns <- all(
      c(
        "investor_name", "portfolio_name", "scenario_name", "scenario_geography",
        "id", "company_name", "ald_sector", "technology", "equity_0_baseline",
        "equity_0_late_sudden", "debt", "volatility", "risk_free_rate", "term"
      ) %in% colnames(data)
    )

    result <- dplyr::tibble(
      investor_name = NA_character_,
      portfolio_name = NA_character_,
      scenario_name = NA_character_,
      scenario_geography = NA_character_,
      id = NA_character_,
      company_name = NA_character_,
      ald_sector = NA_character_,
      technology = NA_character_,
      equity_0_baseline = NA_real_,
      equity_0_late_sudden = NA_real_,
      debt = NA_real_,
      volatility = NA_real_,
      risk_free_rate = NA_real_,
      term = NA_real_,
      Maturity = NA_real_,
      Vt = NA_real_,
      St = NA_real_,
      Dt = NA_real_,
      Survival = NA_real_,
      .rows = nrow(data)
    )
  } else {
    data_has_expected_columns <- all(
      c(
        "investor_name", "portfolio_name", "scenario_name", "scenario_geography",
        "id", "company_name", "ald_sector", "technology", "year",
        "equity_t_baseline", "equity_t_late_sudden", "debt", "volatility",
        "risk_free_rate", "term"
      ) %in% colnames(data)
    )
    stopifnot(data_has_expected_columns)

    result <- dplyr::tibble(
      investor_name = NA_character_,
      portfolio_name = NA_character_,
      scenario_name = NA_character_,
      scenario_geography = NA_character_,
      id = NA_character_,
      company_name = NA_character_,
      ald_sector = NA_character_,
      technology = NA_character_,
      year = NA_integer_,
      equity_t_baseline = NA_real_,
      equity_t_late_sudden = NA_real_,
      debt = NA_real_,
      volatility = NA_real_,
      risk_free_rate = NA_real_,
      term = NA_real_,
      Maturity = NA_real_,
      Vt = NA_real_,
      St = NA_real_,
      Dt = NA_real_,
      Survival = NA_real_,
      .rows = nrow(data)
    )
  }

  result
}
