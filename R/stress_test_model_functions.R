# FIXME: Add tests and documentation


# FIXME: can probably be removed
f <- function(shock_strength_calc) {
  sum(scen_to_follow[1:(position_shock_year + duration_of_shock - 1)]) -
    sum(late_and_sudden[1:(position_shock_year - 1)]) -
    late_and_sudden[position_shock_year - 1] * sum(seq(1, duration_of_shock)) * sum(seq(1, duration_of_shock)) * (100 - shock_strength_calc / 100) -
    (length(late_and_sudden) - (position_shock_year + duration_of_shock) + 1) *
      (scen_to_follow[duration_of_shock + position_shock_year - 1] -
        late_and_sudden[position_shock_year - 1] * (100 + duration_of_shock * shock_strength_calc) / 100)
}



# LATE AND SUDDEN PRICES ----------------------------------------

late_sudden_prices <- function(target_price,
                               baseline_price,
                               year_of_shock,
                               start_year,
                               duration_of_shock) {
  position_shock_year <- year_of_shock - start_year + 1
  ls_price <- baseline_price

  baseline_price_at_shock <- baseline_price[0 + position_shock_year]
  target_price_end_shockperiod <- target_price[duration_of_shock + position_shock_year - 1]

  ls_price[position_shock_year:(position_shock_year + duration_of_shock - 1)] <- zoo::na.approx(c(baseline_price_at_shock, rep(NA, duration_of_shock - 2), target_price_end_shockperiod))

  return(ls_price)
}

join_price_data <- function(df, df_prices) {
  # Joins price data by sector, technology, year
  # scenario_geography NOT YET INCLUDED!
  df <- df %>%
    dplyr::inner_join(df_prices, by = c("technology", "ald_sector", "year"))
}

join_net_profit_margins <- function(df, net_profit_margins) {
  # Joins net profit margins by technology
  df <- df %>%
    dplyr::inner_join(net_profit_margins, by = "technology")
}

calculate_net_profits_old <- function(df) {
  # Calculates annual net profits
  # Input: dataframe that has the baseline & LS production, prices and technoogy net profit margins
  df %>%
    dplyr::mutate(
      net_profits_ls = late_sudden * late_sudden_price * net_profit_margin,
      net_profits_baseline = baseline * Baseline_price * net_profit_margin
    )
}

dcf_model_techlevel <- function(data, discount_rate) {

  # Calculates the annual discounted net profits on technology level
  data %>%
    dplyr::group_by(investor_name, portfolio_name, id, company_name, ald_sector, technology, scenario_geography) %>%
    dplyr::mutate(
      t_calc = seq(0, (dplyr::n() - 1)),
      discounted_net_profit_baseline = net_profits_baseline / (1 + discount_rate)^t_calc,
      discounted_net_profit_ls = net_profits_ls / (1 + discount_rate)^t_calc
    ) %>%
    dplyr::select(-t_calc) %>%
    dplyr::ungroup()
}

# run basic portfolio data consistency checks that are required for further data processing
check_portfolio_consistency <- function(df, start_year) {
  # first year in data set must be the same as start year defined in parameters
  if (min(df$year, na.rm = TRUE) != start_year) {
    stop("Start year of the analysis and first year in the input portfolio do not match.")
  }
  return(df)
}

check_price_consistency <- function(df, start_year) {
  # the year of shock must be greater or equal to the start year of the analysis
  if (!all(df$year >= start_year)) {
    stop(
      "Timerange for price data out of bounds. Past prices cannot be included
      in the further analysis."
    )
  }
  return(df)
}

check_scenario_availability <- function(portfolio, scen_data, scenarios = scenarios) {
  # check that scenarios in portfolio are allowed
  if (!all(portfolio %>% dplyr::pull(scenario) %>% unique() %in% scenarios)) {
    stop(
      "Some scenarios in this data frame are not in the list allowed of scenarios.
      Please check!"
    )
  }
  # check that at least two allowed scenarios remain in portfolio
  if (length(portfolio %>% dplyr::pull(scenario) %>% unique()) < 2) {
    stop(
      "There are less than two allowed scenarios in the portfolio. Stress test
      requires at least two!"
    )
  }
  # check scenarios in portfolio correspond to scenarios in scen data
  if (!all(portfolio %>% dplyr::pull(scenario) %>% unique() %in% (scen_data %>% dplyr::pull(scenario) %>% unique()))) {
    stop(
      "Scenarios differ between portfolio and scenario trajectory data. Check if
      correct inputs were used."
    )
  }
}

# check if the imported scenario data covers every year within the timeframe of analysis
check_scenario_timeframe <- function(scenario_data, start_year = start_year, end_year = end_year) {
  if (!all(seq(start_year, end_year) %in% (scenario_data %>% dplyr::pull(year) %>% unique()))) {
    stop(
      glue::glue(
        "Imported scenario data does not cover the full time frame of the
        analysis. Scenario data must cover at least the period from {start_year}
        to {end_year}"
      )
    )
  } else {
    return(scenario_data)
  }
}
