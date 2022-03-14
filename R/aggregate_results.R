#' Aggregate results
#'
#' Function aggregates results to the expected levels. List element entry
#' `results` is split into market risk results for company and portfolio level.
#'
#' @param results_list A list of results.
#' @param sensitivity_analysis_vars  String vector holding names of iteration
#'   arguments.
#'
#' @return A list of including basic and aggregated results.
aggregate_results <- function(results_list, sensitivity_analysis_vars) {
  sensitivity_analysis_vars <- paste0(sensitivity_analysis_vars, "_arg")

  # value changes -----------------------------------------------------------
  validate_data_has_expected_cols(
    data = results_list$company_technology_value_changes,
    expected_columns = c(
      "investor_name", "portfolio_name", "company_name", "scenario_geography",
      "scenario_name", "year_of_shock", "duration_of_shock", "ald_sector",
      "technology", "VaR_tech_company", sensitivity_analysis_vars
    )
  )

  data <- results_list$company_technology_value_changes %>%
    # TODO: 3384 this is where increasing technologies with zero start value are removed
    dplyr::inner_join(
      results_list$exposure_by_technology_and_company,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "technology", "ald_sector", "scenario_geography", sensitivity_analysis_vars
      )
    )

  data <- data %>%
    dplyr::inner_join(
      results_list$port_aum,
      by = c("investor_name", "portfolio_name", sensitivity_analysis_vars)
    )

  data <- data %>%
    dplyr::mutate(
      tech_company_exposure = .data$asset_portfolio_value * .data$plan_carsten,
      tech_company_value_change = .data$tech_company_exposure * .data$VaR_tech_company / 100
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_company = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      company_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      company_value_change = .data$company_exposure * .data$VaR_company / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$technology, .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_technology = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      technology_value_change = .data$technology_exposure * .data$VaR_technology / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_sector = sum(.data$VaR_technology * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      sector_value_change = .data$sector_exposure * .data$VaR_sector / 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$scenario_geography
    ) %>%
    dplyr::mutate(
      VaR_analysed_sectors = sum(.data$VaR_tech_company * .data$plan_carsten, na.rm = TRUE) /
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_exposure = .data$asset_portfolio_value *
        sum(.data$plan_carsten, na.rm = TRUE),
      analysed_sectors_value_change = .data$analysed_sectors_exposure *
        .data$VaR_analysed_sectors / 100,
      portfolio_aum = .data$asset_portfolio_value,
      # setting portfolio_value_change = analysed_sectors_value_change will
      # underestimate overall impact on portfolio as there can of course be
      # impacts on companies in the portfolio that operate in other sectors
      portfolio_value_change = .data$analysed_sectors_value_change,
      portfolio_value_change_perc = sum(.data$VaR_tech_company * .data$tech_company_exposure, na.rm = TRUE) /
        .data$portfolio_aum
    ) %>%
    dplyr::ungroup()

  company_value_changes <- data %>%
    dplyr::select(-c(.data$plan_carsten, .data$plan_sec_carsten, .data$year))

  exposure_at_default_credit <- results_list$exposure_by_technology_and_company %>%
    # distinct in order to remove unused technology level information, the
    # trivial year and the unneeded financial info
    dplyr::distinct(
      .data$investor_name, .data$portfolio_name, .data$company_name,
      .data$ald_sector, .data$scenario_geography, .data$plan_sec_carsten
    )

  # TODO: needs a check which lines have been removed
  company_pd_changes_annual <- results_list$company_pd_changes_annual %>%
    dplyr::inner_join(
      exposure_at_default_credit,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "scenario_geography", "ald_sector"
      )
    )

  company_pd_changes_annual <- company_pd_changes_annual %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$scenario_geography, .data$year
    ) %>%
    dplyr::mutate(
      PD_baseline_sector = stats::weighted.mean(
        .data$PD_baseline,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_late_sudden_sector = stats::weighted.mean(
        .data$PD_late_sudden,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_change_sector = stats::weighted.mean(
        .data$PD_change,
        w = .data$plan_sec_carsten, na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()

  # TODO: needs a check which lines have been removed
  company_pd_changes_overall <- results_list$company_pd_changes_overall %>%
    dplyr::inner_join(
      exposure_at_default_credit,
      by = c(
        "investor_name", "portfolio_name", "company_name",
        "scenario_geography", "ald_sector"
      )
    )

  company_pd_changes_overall <- company_pd_changes_overall %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$ald_sector,
      .data$scenario_geography, .data$term
    ) %>%
    dplyr::mutate(
      PD_baseline_sector = stats::weighted.mean(
        .data$PD_baseline,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_late_sudden_sector = stats::weighted.mean(
        .data$PD_late_sudden,
        w = .data$plan_sec_carsten, na.rm = TRUE
      ),
      PD_change_sector = stats::weighted.mean(
        .data$PD_change,
        w = .data$plan_sec_carsten, na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()

  return(list(
    company_value_changes = company_value_changes,
    company_expected_loss = results_list$company_expected_loss,
    company_pd_changes_annual = company_pd_changes_annual,
    company_pd_changes_overall = company_pd_changes_overall,
    company_trajectories = results_list$company_trajectories
  ))
}
