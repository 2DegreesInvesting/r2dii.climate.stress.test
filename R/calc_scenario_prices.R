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
#' @param target_scenario String holding name of the target scenario.
#' @param transition_scenario Tibble with 1 row holding at least variables
#'   `year_of_shock` and `duration_of_shock`.
#' @param start_year Start_year of analysis
#'
#' @return A tibble holding late_and_sudden_prices
calc_scenario_prices <- function(price_data, baseline_scenario, target_scenario,
                                 transition_scenario, start_year) {
  data <- price_data %>%
    dplyr::mutate(Baseline_price = !!rlang::sym(paste0("price_", baseline_scenario))) %>%
    # NOTE: deviating from lower snake case here due legacy functions
    dplyr::mutate(target_price = !!rlang::sym(paste0("price_", target_scenario))) %>%
    dplyr::group_by(ald_sector, ald_business_unit) %>%
    dplyr::mutate(
      late_sudden_price = late_sudden_prices(
        target_price = .data$target_price,
        baseline_price = .data$Baseline_price,
        year_of_shock = transition_scenario$year_of_shock,
        start_year = start_year,
        duration_of_shock = transition_scenario$duration_of_shock
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, ald_sector, ald_business_unit, Baseline_price, late_sudden_price)

  return(data)
}
