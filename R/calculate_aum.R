#' Calculate aum
#'
#' Function calculated asset under management for portfolio from sector
#' exposures.
#'
#' @param sector_exposures A tibble holding sector exposure.
#'
#' @return A tibble holding columns investor_name, portfolio_name and
#'   asset_portfolio_value.
calculate_aum <- function(sector_exposures) {
  aum <- sector_exposures %>%
    dplyr::group_by(investor_name, portfolio_name) %>%
    dplyr::summarise(
      asset_portfolio_value = sum(valid_value_usd),
      .groups = "drop"
    )

  if (aum$asset_portfolio_value <= 0) {
    stop("Asset under management has negative or 0 value. This is not supported by the analysis.", call. = FALSE)
  }

  return(aum)
}
