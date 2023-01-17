#' Calculates annual net profits on the company-technology level for the
#' baseline and late and sudden scenarios. Climate laggards which need to build
#' out their production in increasing technologies to compensate for their
#' missed targets, are "punished" by adjusting the net profit margin on their
#' additional build out based on their proximity to target within the given
#' technology. Specifically, we measure the ratio of how much of the required
#' build out or reduction in a technology the company will have done at the end
#' of the forecast period. If the technology has an increasing target and the
#' ratio of completion is below one, the net_profit_margin on the additional
#' production build out is multiplied with the proximity to the target. This
#' approximates the additional capital investment such a company would have to
#' make in a short time, which leads to added costs. This ensures that late
#' build out will not proportionally translate into increased profits.
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the technology.
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario.
#' @param end_year Numeric, holding end year of analysis.
#' @param carbon_data NGFS carbon prices.
calculate_net_profits <- function(data,
                                  shock_year,
                                  end_year,
                                  carbon_data) {

  market_passthrough <- 0

  data <- data %>%
    merge(carbon_data, by = c("year")) %>%
    dplyr::mutate(
      carbon_tax = ifelse(year > shock_year, .data$carbon_tax, 0),
      production_compensation = .data$late_sudden - .data$baseline,
      net_profits_baseline = .data$baseline * .data$Baseline_price * .data$net_profit_margin,
      net_profits_ls = dplyr::if_else(
        .data$direction == "declining",
        .data$late_sudden * (.data$late_sudden_price - (1 - market_passthrough) * .data$carbon_tax * .data$emission_factor) * .data$net_profit_margin,
        .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
          .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)
        # TODO: ADO4109 - should the market size penalty only be applied to laggards?
      )
    )
}

calculate_net_profits_without_carbon_tax <- function(data) {
  data <- data %>%
    dplyr::mutate(
      production_compensation = .data$late_sudden - .data$baseline,
      net_profits_baseline = .data$baseline * .data$Baseline_price * .data$net_profit_margin,
      net_profits_ls = dplyr::if_else(
        .data$direction == "declining",
        .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin,
        .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
          .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)
        # TODO: ADO4109 - should the market size penalty only be applied to laggards?
      )
    )
}



#' Calculates discounted net profits based on a dividends discount model
#'
#' @param data A data frame containing the annual net profits on company
#'   technology level
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF.
dividend_discount_model <- function(data, discount_rate) {
  data <- data %>%
    dplyr::group_by(
      .data$id, .data$company_name, .data$ald_sector, .data$technology,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      t_calc = seq(0, (dplyr::n() - 1)),
      discounted_net_profit_baseline = .data$net_profits_baseline /
        (1 + .env$discount_rate)^.data$t_calc,
      discounted_net_profit_ls = .data$net_profits_ls /
        (1 + .env$discount_rate)^.data$t_calc
    ) %>%
    dplyr::select(-.data$t_calc) %>%
    dplyr::ungroup()

  return(data)
}
