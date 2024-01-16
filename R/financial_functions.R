#' Calculates annual net profits on the company-ald_business_unit level for the
#' baseline and late and sudden scenarios. Climate laggards which need to build
#' out their production in increasing technologies to compensate for their
#' missed targets, are "punished" by adjusting the net profit margin on their
#' additional build out based on their proximity to target within the given
#' ald_business_unit. Specifically, we measure the ratio of how much of the required
#' build out or reduction in a ald_business_unit the company will have done at the end
#' of the forecast period. If the ald_business_unit has an increasing target and the
#' ratio of completion is below one, the net_profit_margin on the additional
#' production build out is multiplied with the proximity to the target. This
#' approximates the additional capital investment such a company would have to
#' make in a short time, which leads to added costs. This ensures that late
#' build out will not proportionally translate into increased profits.
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the ald_business_unit.
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario.
#' @param market_passthrough A firm's ability to pass a carbon tax onto the consumer.
#' @param financial_stimulus Additional channel through which the net profits of green
#' companies can be boosted under a shock scenario.
#' @param carbon_data NGFS carbon prices.
calculate_net_profits <- function(data,
                                  carbon_data,
                                  shock_year,
                                  market_passthrough,
                                  financial_stimulus) {

  baseline <- calculate_net_profits_baseline(data) %>%
    dplyr::select(
      .data$company_id,
      .data$year,
      .data$scenario_geography,
      .data$ald_sector,
      .data$ald_business_unit,
      .data$net_profits_baseline
    )


  shock_increasing_technologies <- calculate_net_profits_shock_increasing_technologies(data = data %>% dplyr::filter(.data$direction == "increasing"),
                                                                                       financial_stimulus = financial_stimulus,
                                                                                       shock_year = shock_year)

  shock_declining_technologies <- calculate_net_profits_shock_declining_technologies_carbon_tax(
    data = data %>% dplyr::filter(.data$direction == "declining"),
    carbon_data = carbon_data,
    shock_year = shock_year,
    market_passthrough = market_passthrough
  )

  data <- dplyr::bind_rows(shock_increasing_technologies, shock_declining_technologies)

  data <- dplyr::full_join(
    data,
    baseline,
    by = c(
      "company_id",
      "year",
      "scenario_geography",
      "ald_sector",
      "ald_business_unit"
    )
  )

  return(data)
}

#' Calculates annual net profits on the company-ald_business_unit level for the
#' baseline and late and sudden scenarios - with a carbon tax being added.
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the ald_business_unit.
#' @param shock_year A numeric vector of length one that indicates in which year
#'   the policy shock strikes in a given scenario.
#' @param carbon_data  NGFS carbon prices.
#' @param market_passthrough A firm's ability to pass a carbon tax onto the consumer.
#'
#' @return Data frame with annual netprofits for all cases without carbon tax.

calculate_net_profits_shock_declining_technologies_carbon_tax <- function(data, shock_year,
                                                                          carbon_data, market_passthrough) {

  carbon_data$carbon_tax <- ifelse(
    carbon_data$scenario == "increasing_carbon_tax_50" & carbon_data$year < shock_year, 0,
    ifelse(
      carbon_data$scenario == "increasing_carbon_tax_50" & carbon_data$year == shock_year, 50,
      ifelse(
        carbon_data$scenario == "increasing_carbon_tax_50", carbon_data$carbon_tax * (1.04)^ (carbon_data$year - shock_year),
        carbon_data$carbon_tax
      )
    )
  )

  data <- data %>%
    merge(carbon_data, by = c("year"))

  data_over_shoot_increasing <- data %>% dplyr::filter(.data$overshoot_direction == "Increasing") %>%
    dplyr::mutate(
      production_compensation = .data$late_sudden - .data$baseline,
      carbon_tax = ifelse(.data$year < shock_year, 0, .data$carbon_tax),
      net_profits_ls = .data$late_sudden * (.data$late_sudden_price -
                                              (1 - market_passthrough) * .data$carbon_tax * .data$emission_factor) * .data$net_profit_margin -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)) %>%
    dplyr::select(-c("proximity_to_target", "production_compensation"))


  data_over_shoot_decreasing <- data %>% dplyr::filter(.data$overshoot_direction == "Decreasing") %>%
    dplyr::mutate(
      production_compensation = 0,
      carbon_tax = ifelse(.data$year < shock_year, 0, .data$carbon_tax),
      net_profits_ls = .data$late_sudden * (.data$late_sudden_price -
                                              (1 - market_passthrough) * .data$carbon_tax * .data$emission_factor) * .data$net_profit_margin --
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)) %>%
    dplyr::select(-c("proximity_to_target", "production_compensation"))


  data <- dplyr::bind_rows(data_over_shoot_increasing, data_over_shoot_decreasing)

  data$net_profits_ls[data$net_profits_ls < 0] <- 0

  return(data)
}


#' Calculates annual net profits on the company-ald_business_unit level for the
#' baseline and late and sudden scenarios - without a carbon tax being added.
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the ald_business_unit.
#'
#' @return Data frame with annual netprofits for all cases without carbon tax
calculate_net_profits_without_carbon_tax <- function(data) {
  baseline <- calculate_net_profits_baseline(data)
  shock_increasing_technologies <- calculate_net_profits_shock_increasing_technologies_without_carbon_tax(data = data %>% dplyr::filter(.data$direction == "increasing"))
  shock_declining_technologies <- calculate_net_profits_shock_declining_technologies_without_carbon_tax(data = data %>% dplyr::filter(.data$direction == "declining"))

  data <- dplyr::full_join(shock_increasing_technologies, shock_declining_technologies)
  data <- dplyr::full_join(data, baseline)

  return(data)
}


#' Calculates annual net profits on the company-ald_business_unit level for the baseline scenario
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the ald_business_unit.
#'
#' @return A data frame with net profits for the baseline scenario.

calculate_net_profits_baseline <- function(data) {
  data <- data %>%
    dplyr::mutate(net_profits_baseline = .data$baseline * .data$Baseline_price * .data$net_profit_margin)

  return(data)
}


#' Calculates annual net profits on the company-ald_business_unit level for the shock scenario for declining technologies
#'
#' @param data A data frame containing the production forecasts of companies
#'   under baseline and late and sudden, market prices/costs, company net profit
#'   margins, the proximity to target in the production forecast period and an
#'   indication of the direction of the ald_business_unit.
#'
#' @return A data frame with net profits of companies with a declining ald_business_unit

calculate_net_profits_shock_declining_technologies <- function(data) {

  data <- data %>%
    dplyr::mutate(net_profits_ls = .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin) #%>%
   # dplyr::select(-c("proximity_to_target"))

  return(data)
}



calculate_net_profits_shock_declining_technologies_without_carbon_tax <- function(data) {

  data <- data %>%
    dplyr::mutate(net_profits_ls = .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin)

  return(data)
}


#' Calculates annual net profits on the company-ald_business_unit level for the shock scenario for increasing technologies.
#' Climate laggards which need to build out their production in increasing technologies to compensate for their
#' missed targets, are "punished" by adjusting the net profit margin on their
#' additional build out based on their proximity to target within the given
#' ald_business_unit. Specifically, we measure the ratio of how much of the required
#' build out or reduction in a ald_business_unit the company will have done at the end
#' of the forecast period. If the ald_business_unit has an increasing target and the
#' ratio of completion is below one, the net_profit_margin on the additional production build out
#' is multiplied with the proximity to the target. This approximates the additional capital investment
#' such a company would have to make in a short time, which leads to added costs. This ensures that late
#' build out will not proportionally translate into increased profits.
#'
#' @param data A data frame containing the production forecasts of companies with increasing under the late and sudden,
#' market prices/costs, company net profit margins, the proximity to target in the production forecast period and an
#' indication of the direction of the ald_business_unit.
#' @param financial_stimulus Additional channel through which the net profits of green
#' companies can be boosted under a shock scenario
#' @param shock_year Year of the shock.
#'
#' @return  A data frame with net profits of companies with a increasing ald_business_unit

calculate_net_profits_shock_increasing_technologies <- function(data, financial_stimulus, shock_year) {

  data_overshoot_increasing <- data %>% dplyr::filter(.data$overshoot_direction == "Increasing") %>%
    dplyr::mutate(
      production_compensation = .data$late_sudden - .data$baseline,
      financial_stimulus = dplyr::if_else(.data$year < shock_year, 1, financial_stimulus),
      net_profits_ls = (.data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)) * financial_stimulus
    )  %>%
   dplyr::select(-c("proximity_to_target", "production_compensation"))

  data_overshoot_decreasing <- data %>% dplyr::filter(.data$overshoot_direction == "Decreasing") %>%
    dplyr::mutate(
      production_compensation = 0,
      financial_stimulus = dplyr::if_else(.data$year < shock_year, 1, financial_stimulus),
      net_profits_ls = (.data$late_sudden * .data$late_sudden_price * .data$net_profit_margin  -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)) * financial_stimulus
    )  %>%
    dplyr::select(-c("proximity_to_target", "production_compensation"))

  data <- dplyr::bind_rows(data_overshoot_increasing, data_overshoot_decreasing)

  return(data)
}

calculate_net_profits_shock_increasing_technologies_without_carbon_tax <- function(data) {

  data <- data %>%
    dplyr::mutate(
      production_compensation = .data$late_sudden - .data$baseline,
      net_profits_ls = .data$late_sudden * .data$late_sudden_price * .data$net_profit_margin -
        .data$production_compensation * .data$late_sudden_price * .data$net_profit_margin * (1 - .data$proximity_to_target)
    )

return(data)

}

#' Calculates discounted net profits based on a dividends discount model
#'
#' @param data A data frame containing the annual net profits on company
#'   ald_business_unit level
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF.
dividend_discount_model <- function(data, discount_rate) {
  data <- data %>%
    dplyr::group_by(
      .data$company_id, .data$company_name, .data$ald_sector, .data$ald_business_unit,
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
