#' Calculate change of expected loss between scenarios for corporate loans on
#' the company-technology level. The function uses a maximum term of 5 years and
#' all loans with longer maturities are thrown into the 5 year bucket. The
#' calculation of PD changes that underlies the input to this function is based
#' on the comparison of overall NPVs of the corresponding equity values for the
#' companies at hand. That is, the PD changes take into account all discounted
#' future profits plus the terminal value.
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param loss_given_default A numeric vector of length one that indicates
#'   the loss given default for the given asset type. Usually either a senior
#'   claim or a subordinated claim.
#' @param exposure_at_default A dataframe that contains the share of the
#'   portfolio value of each company-technology combination. Used to quantify
#'   the impact of the company-tech level shock on higher levels of aggregation
#'   in the portfolio
#' @param port_aum A dataframe containing the value of the portfolio for
#'   the asset type at hand
company_expected_loss <- function(data,
                                  loss_given_default = NULL,
                                  exposure_at_default = NULL,
                                  port_aum = NULL) {
  force(data)
  loss_given_default %||% stop("Must provide input for 'loss_given_default'", call. = FALSE)
  exposure_at_default %||% stop("Must provide input for 'exposure_at_default'", call. = FALSE)
  port_aum %||% stop("Must provide input for 'port_aum'", call. = FALSE)

  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "ald_sector", "scenario_name", "PD_baseline", "PD_late_sudden",
      "PD_change"
    )
  )

  validate_data_has_expected_cols(
    data = exposure_at_default,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "plan_carsten", "plan_sec_carsten", "term", "pd"
    )
  )

  company_exposure <- exposure_at_default %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$ald_sector, .data$scenario_geography,
      .data$term, .data$pd
    ) %>%
    dplyr::summarise(
      percent_exposure = sum(.data$plan_carsten, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  company_exposure <- company_exposure %>%
    dplyr::inner_join(port_aum, by = c("investor_name", "portfolio_name"))

  company_exposure <- company_exposure %>%
    dplyr::mutate(
      exposure_at_default = .data$asset_portfolio_value * .data$percent_exposure
    )

  data <- data %>%
    dplyr::mutate(lgd = loss_given_default)

  data <- data %>%
    dplyr::inner_join(
      company_exposure,
      by = c(
        "investor_name", "portfolio_name", "company_name", "ald_sector",
        "scenario_geography", "term"
      )
    )

  data <- data %>%
    dplyr::mutate(
      expected_loss_baseline = .data$pd *
        .data$lgd *
        .data$exposure_at_default,
      expected_loss_late_sudden = (.data$pd + .data$PD_change) *
        .data$lgd *
        .data$exposure_at_default
    )
}
