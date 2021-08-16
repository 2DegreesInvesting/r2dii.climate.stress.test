#' Calculate annual net profits under baseline and late & sudden scenarios.
#'
#' @description This function calculates the annual net profits for the baseline
#' and the late & sudden shock scenarios for each company-technology combination.
#' It is based on the production trajectories, market prices (IEA by default),
#' company net profit margins (regional/sector averages, where company level
#' not known), forward looking emission factors and a carbon tax (defaults to
#' NGFS carbon prices). We model the the production channel and the price
#' channel. More details can be obtained in the general documentation.
#'
#' @param data A dataframe containing the annual production and price data
#' @param carbon_price_data A dataframe containing the annual development of
#'   carbon prices under different scenarios.
#' @param baseline_tax A character vector of length 1, specifying the baseline
#'   scenario for the carbon tax.
#' @param late_sudden_tax A character vector of length 1, specifying the late &
#'   sudden scenario for the carbon tax.
calculate_annual_net_profits <- function(data,
                                         carbon_price_data = NULL,
                                         baseline_tax = NULL,
                                         late_sudden_tax = NULL) {
  force(data)
  carbon_price_data %||% stop("Must provide input for 'carbon_price_data'", call. = FALSE)
  baseline_tax %||% stop("Must provide input for 'baseline_tax'", call. = FALSE)
  late_sudden_tax %||% stop("Must provide input for 'late_sudden_tax'", call. = FALSE)

  #TODO: add checks for content, time frame etc..

  data_has_expected_columns <- all(
    c(
      "year", "baseline", "Baseline_price", "late_sudden", "late_sudden_price",
      "net_profit_margin", "ald_emissions_factor", "scenario_geography"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  carbon_price_data_has_expected_columns <- all(
    c(
      "year", "scenario", "scenario_geography", "unit", "carbon_tax"
    ) %in% colnames(carbon_price_data)
  )
  stopifnot(carbon_price_data_has_expected_columns)

  carbon_price_data <- carbon_price_data %>%
    tidyr::pivot_wider(
      id_cols = c(.data$year, .data$scenario_geography, .data$variable, .data$unit),
      names_from = .data$scenario,
      values_from = .data$carbon_tax
    ) %>%
    dplyr::select(
      .data$year, .data$scenario_geography, .data$unit,
      !!rlang::sym(baseline_tax), !!rlang::sym(late_sudden_tax)
    ) %>%
    dplyr::rename(
      unit_carbon_tax = .data$unit,
      baseline_tax = !!rlang::sym(baseline_tax),
      late_sudden_tax = !!rlang::sym(late_sudden_tax)
    )

  data <- data %>%
    left_join(
      carbon_price_data,
      by = c("year", "scenario_geography")
    )

  data <- data %>%
    dplyr::mutate(
      net_profits_baseline = .data$baseline * .data$Baseline_price * .data$net_profit_margin -
        .data$ald_emissions_factor * .data$baseline * .data$baseline_tax,
      net_profits_ls = .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
        .data$ald_emissions_factor * .data$late_sudden * .data$late_sudden_tax
    )

  return(data)

}
