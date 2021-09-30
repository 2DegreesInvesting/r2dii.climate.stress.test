calculate_aum <- function(sector_exposures) {
  aum <- sector_exposures %>%
    group_by(investor_name, portfolio_name) %>%
    summarise(
      asset_portfolio_value = sum(valid_value_usd),
      .groups = "drop_last"
    )
  return(aum)
}
