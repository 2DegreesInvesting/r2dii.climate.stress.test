#' Calculate late and sudden prices
#'
#' Wrapper function around call to [late_sudden_prices()].
#'
#' @param price_data A tibble holding price data.
#' @param baseline_scenario String holding name of the baseline scenario.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Numeric, holding start year of analysis.
#'
#' @return A tibble holding late_and_sudden_prices
calc_late_sudden_prices <- function(price_data, baseline_scenario, transition_scenario, start_year) {
  late_sudden_prices_data <- price_data %>%
    dplyr::mutate(Baseline = !!rlang::sym(baseline_scenario)) %>%
    dplyr::rename(
      year = year, ald_sector = sector, technology = technology, NPS_price = NPS,
      SDS_price = SDS, Baseline_price = Baseline, B2DS_price = B2DS
    ) %>%
    dplyr::group_by(ald_sector, technology) %>%
    dplyr::mutate(
      late_sudden_price = late_sudden_prices(
        SDS_price = SDS_price,
        Baseline_price = Baseline_price,
        year_of_shock = transition_scenario$year_of_shock,
        start_year = start_year,
        duration_of_shock = transition_scenario$duration_of_shock
      )
    ) %>%
    dplyr::ungroup()

  return(late_sudden_prices_data)
}
