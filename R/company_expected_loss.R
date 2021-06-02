#' Calculate percentage value change between scenarios for equity (and
#' temporarily other asset types) on the company-technology level
#'
#' @param data A dataframe containing the (discounted) annual profits
#' @param loss_given_default A dataframe containing sectoral percentages of how
#'   large the share of lost value in a loan is, in case of default.
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

  data_has_expected_columns <- all(
    c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "ald_sector", "scenario_name", "PD_baseline", "PD_late_sudden",
      "PD_change"
    ) %in% colnames(data)
  )
  stopifnot(data_has_expected_columns)

  loss_given_default_has_expected_columns <- all(
    c(
      "sector", "lgd"
    ) %in% colnames(loss_given_default)
  )
  stopifnot(loss_given_default_has_expected_columns)

  exposure_at_default_has_expected_columns <- all(
    c(
      "investor_name", "portfolio_name", "company_name", "year",
      "scenario_geography", "ald_sector", "technology",
      "plan_carsten", "plan_sec_carsten", "term", "PD_0"
    ) %in% colnames(exposure_at_default)
  )
  stopifnot(exposure_at_default_has_expected_columns)

  company_exposure <- exposure_at_default %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$ald_sector, .data$technology, .data$scenario_geography,
      .data$term, .data$PD_0
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
    dplyr::inner_join(loss_given_default, by = c("ald_sector" = "sector"))

  data <- data %>%
    dplyr::left_join(
      company_exposure,
      by = c(
        "investor_name", "portfolio_name", "company_name", "ald_sector",
        "technology", "scenario_geography", "term"
      )
    )

  data <- data %>%
    dplyr::mutate(
      expected_loss_baseline = .data$PD_0 *
        .data$lgd *
        .data$exposure_at_default,
      expected_loss_late_sudden = .data$PD_0  *
        (1 + .data$PD_change) *
        .data$lgd *
        .data$exposure_at_default
    )
}
