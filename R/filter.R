#' Keep rows that fulfill constraints of the merton model
#'
#' Keep rows that fulfill constraints of the merton model as line out for
#' [calc_survival_probability_merton()].
#'
#' @param data A tibble holding at least the columns `debt`,
#'   `equity_0_baseline` or `equity_t_baseline`, `equity_0_late_sudden` or `equity_t_late_sudden`, `volatility`, `risk_free_rate`
#'   and `term`.
#' @param stage String, indicating if checks are done for overall or annual
#'   results.
#'
#' @return Tibble holding rows from `data` that are compatible with constraints
#'  of [calc_survival_probability_merton()].
keep_merton_compatible_rows <- function(data, stage) {
  if (!stage %in% c("overall", "annual")) {
    stop("Invalid value for argument stage")
  }

  if (stage == "overall") {
    data <- data %>%
      dplyr::mutate(V0_base = .data$debt + .data$equity_0_baseline) %>%
      dplyr::mutate(V0_late_sudden = .data$debt + .data$equity_0_late_sudden)
  } else {
    data <- data %>%
      dplyr::mutate(V0_base = .data$debt + .data$equity_t_baseline) %>%
      dplyr::mutate(V0_late_sudden = .data$debt + .data$equity_t_late_sudden)
  }

  data_filtered <- data %>%
    dplyr::filter(.data$risk_free_rate >= 0) %>%
    dplyr::filter(.data$debt > 0 & .data$V0_base > 0 & .data$V0_late_sudden > 0 & .data$volatility > 0 & .data$term > 0) %>%
    dplyr::select(-all_of(c("V0_base", "V0_late_sudden")))

  if (nrow(data_filtered) < nrow(data)) {
    cat(paste0("Removed ", nrow(data) - nrow(data_filtered), " rows when checking for compatibility with merton model. \n"))

    if (nrow(data_filtered) == 0) {
      stop("No data remain after removing rows that are not compatible with merton model.")
    }
  }
  return(data_filtered)
}
