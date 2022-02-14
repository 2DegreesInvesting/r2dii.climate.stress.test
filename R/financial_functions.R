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
calculate_net_profits <- function(data) {
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
