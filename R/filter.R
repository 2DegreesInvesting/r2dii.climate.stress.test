#' Keep rows that fulfill constraints of the merton model
#'
#' Keep rows that fulfill constraints of the merton model as line out for
#' [calc_survival_probability_merton()].
#'
#' @param data A tibble holding at least the columns `debt`, `equity_0_baseline`,
#' `equity_0_late_sudden`, `volatility`, `risk_free_rate` and `term`.
#'
#'@return Tibble holding rows from `data` that are compatible with constraints
#'  of [calc_survival_probability_merton()].
keep_merton_compatible_rows <- function(data) {

}
