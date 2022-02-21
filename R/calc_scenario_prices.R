#' Calculate scenario prices
#'
#' Function generates prices for baseline and late and sudden shock scenario.
#' Price for baseline scenario correspond to prices of `baseline_scenario`.
#' Prices for the late sudden scenario also correspond to `baseline_scenario`
#' until the `year_of_shock`. From then on they linearly approach the price
#' level of the `shock_scenario` during the `duration_of_shock`.
#'
#'
#' @param price_data A tibble holding price data.
#' @param baseline_scenario String holding name of the baseline scenario.
#' @param shock_scenario String holding name of the shock scenario.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#'
#' @return A tibble holding late_and_sudden_prices
calc_scenario_prices <- function(price_data, baseline_scenario, shock_scenario, transition_scenario) {

  data <- price_data %>%
    dplyr::mutate(Baseline = !!rlang::sym(paste0(baseline_scenario, "_price"))) %>%
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

  return(data)
}
