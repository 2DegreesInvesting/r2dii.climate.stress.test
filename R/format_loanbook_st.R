#' Transforms the PACTA for banks output to PACTA EQ/CB format to enable use
#' in stress test model.
#'
#' @description
#' Takes as input the results from a PACTA for banks run, augmented with credit
#' sizes (outstanding and limits), as provided by running the calc_loan_book.R
#' script. This data set is wrangled so that it fits the structure normally
#' produced by PACTA_analysis. Such a formatting is required in order to allow
#' seamless integration of PACTA for banks results (i.e. loan book alignment
#' data) with the climate transition risk stress test / scenario analysis.
#'
#' @param data A dataframe that contains augmented PACTA for banks results for
#'   the project at hand. This dataframe must additionally contain information
#'   on credit size (outstanding and limit).
#' @param investor_name Character. A string that indicates for which investor
#'   the data should be transformed.
#' @param portfolio_name Character. A string that indicates for which portfolio
#'   the data should be transformed. Must be a portfolio that belongs to the
#'   indicated investor.
#' @param equity_market Character. A string that indicates the location of the
#'   equity market of the analysis. This is required to match the P4I data
#'   structure and should use the equity_market_filter passed form the user
#'   function as input.
#' @param credit Character. Indicates whether the formatted output should return
#'   data for outstanding credits or credit limits. The stress test will then be
#'   run on the chosen indicator.
#'
#' @family utility functions
#'
#' @return data frame
format_loanbook_st <- function(data,
                               investor_name,
                               portfolio_name,
                               equity_market,
                               credit) {
  data_has_required_cols <- all(c(
    "sector",
    "technology",
    "year",
    "region",
    "scenario_source",
    "name_ald",
    "metric",
    "production_unweighted",
    "technology_share",
    "loan_share_outstanding",
    "loan_share_credit_limit"
  ) %in% colnames(data))

  stopifnot(data_has_required_cols)

  group_vars <- c(
    "investor_name",
    "portfolio_name",
    "scenario_source",
    "scenario",
    "allocation",
    "equity_market",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology",
    "company_name"
  )

  credit <- glue::glue("loan_share_{credit}")

  results_loanbook <- data %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = investor_name,
      allocation = allocation_method_lookup,
      # ADO 1933 - there is no actual equity market for loans, this variable is
      # added as a placeholder to match the P4I data structure
      equity_market = equity_market,
      credit_choice = !!rlang::sym(credit),
      plan_carsten = .data$technology_share * .data$credit_choice,
      plan_tech_prod = .data$production_unweighted,
      scen_tech_prod = .data$production_unweighted
    ) %>%
    dplyr::rename(
      scenario = .data$metric,
      scenario_geography = .data$region,
      ald_sector = .data$sector,
      company_name = .data$name_ald
    )

  results_loanbook <- results_loanbook %>%
    dplyr::select(
      -c(
        .data$loan_share_outstanding, .data$loan_share_credit_limit,
        .data$production_unweighted, .data$technology_share
      )
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_vars[group_vars != "technology"])) %>%
    dplyr::mutate(
      scenario_geography = stringr::str_to_title(.data$scenario_geography),
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      plan_sec_carsten = sum(.data$plan_carsten, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!!rlang::syms(group_vars),
      .data$plan_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$plan_sec_prod, .data$plan_sec_carsten
    )

  # custom formatting to adapt to P4I style scenanrio data
  results_loanbook <- results_loanbook %>%
    dplyr::mutate(scenario_geography = gsub(" ", "", scenario_geography, fixed = TRUE)) %>%
    dplyr::mutate(scenario_geography = dplyr::case_when(
      .data$scenario_geography == "CentralAndSouthAmerica" ~ "CentralandSouthAmerica",
      .data$scenario_geography == "EuropeanUnion" ~ "EU",
      .data$scenario_geography == "NonOecd" ~ "NonOECD",
      .data$scenario_geography == "Oecd" ~ "OECD",
      .data$scenario_geography == "UnitedStates" ~ "US",
      TRUE ~ .data$scenario_geography
    ))

  plan <- results_loanbook %>%
    # scenario == "projected" corresponds to company production plans in P4B
    dplyr::filter(.data$scenario == "projected") %>%
    dplyr::select(-.data$scen_tech_prod)

  scen <- results_loanbook %>%
    dplyr::filter(.data$scenario %in% p4b_scenarios_lookup) %>%
    dplyr::select(
      -c(
        .data$plan_tech_prod, .data$plan_carsten,
        .data$plan_sec_prod, .data$plan_sec_carsten
      )
    )

  results_loanbook <- scen %>%
    dplyr::inner_join(
      plan,
      by = c(group_vars[group_vars != "scenario"])
    ) %>%
    dplyr::select(-.data$scenario.y) %>%
    dplyr::rename(scenario = .data$scenario.x) %>%
    dplyr::relocate(
      !!!rlang::syms(group_vars),
      .data$plan_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$plan_sec_prod, .data$plan_sec_carsten
    ) %>%
    dplyr::inner_join(p4i_p4b_scenario_lookup, by = c("scenario" = "scenario_p4b")) %>%
    dplyr::mutate(
      scenario = .data$scenario_p4i,
      scenario_source = stringr::str_to_upper(
        stringr::str_remove(.data$scenario_source, "_")
      )
    ) %>%
    # adjusting scenario column to hold source_scenario, to be compatible with PACTA
    # results for EQ and CB
    dplyr::mutate(scenario = paste(.data$scenario_source, .data$scenario, sep = "_")) %>%
    dplyr::inner_join(
      p4i_p4b_sector_technology_lookup,
      by = c("ald_sector" = "sector_p4b", "technology" = "technology_p4b")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4i,
      technology = .data$technology_p4i
    ) %>%
    # ADO 1933 - add temporary placeholder for id to make the columns consistent with P4I
    # For loans, this variable should not be used for anything and not be filtered out
    dplyr::mutate(id = NA_real_) %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$scenario,
      .data$allocation, .data$id, .data$company_name, .data$equity_market,
      .data$scenario_geography, .data$year, .data$ald_sector, .data$technology,
      .data$plan_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$plan_sec_prod, .data$plan_sec_carsten
    )

  output_has_required_cols <- all(c(
    "investor_name",
    "portfolio_name",
    "scenario",
    "allocation",
    "id",
    "company_name",
    "equity_market",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology",
    "plan_tech_prod",
    "plan_carsten",
    "scen_tech_prod",
    "plan_sec_prod",
    "plan_sec_carsten"
  ) %in% colnames(results_loanbook))

  stopifnot(output_has_required_cols)

  return(results_loanbook)
}
