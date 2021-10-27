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
#'
#' @export
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
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::mutate(
      # TODO: This ensure the first letter is written as capital.
      # May not be safe for geographies with more than one word
      scenario_geography = paste0(toupper(substr(.data$scenario_geography, 1, 1)), substr(.data$scenario_geography, 2, nchar(.data$scenario_geography))),
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      plan_sec_carsten = sum(.data$plan_carsten, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!!rlang::syms(group_vars),
      .data$plan_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$plan_sec_prod, .data$plan_sec_carsten
    )

  plan <- results_loanbook %>%
    # scenario == "projected" corresponds to company production plans in P4B
    dplyr::filter(.data$scenario == "projected") %>%
    dplyr::select(-.data$scen_tech_prod)

  scen <- results_loanbook %>%
    # TODO: this should be a list in lookup or similar
    dplyr::filter(.data$scenario %in% c("target_b2ds", "target_cps", "target_rts", "target_sps", "target_steps", "target_2ds", "target_sds")) %>% # TODO: pass corporate_economy and filter in workflow?
    dplyr::select(
      -c(
        .data$plan_tech_prod, .data$plan_carsten,
        .data$plan_sec_prod, .data$plan_sec_carsten
      )
    )

  # TODO: re-map the scenario names
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
    dplyr::mutate(
      # TODO: this should be extracted into mapping file
      scenario = dplyr::case_when(
        .data$scenario == "target_cps" ~ "CPS",
        .data$scenario == "target_rts" ~ "NPS",
        .data$scenario == "target_sps" ~ "NPS",
        .data$scenario == "target_steps" ~ "NPS",
        .data$scenario == "target_2ds" ~ "SDS",
        .data$scenario == "target_sds" ~ "SDS",
        .data$scenario == "target_b2ds" ~ "B2DS",
        TRUE ~ .data$scenario
      ),
      scenario_source = stringr::str_to_upper(
        stringr::str_remove(.data$scenario_source, "_")
      ),
      # TODO: this should be extracted into mapping file
      ald_sector = dplyr::case_when(
        .data$ald_sector == "power" ~ "Power",
        .data$ald_sector == "oil and gas" ~ "Oil&Gas",
        .data$ald_sector == "coal" ~ "Coal",
        .data$ald_sector == "automotive" ~ "Automotive",
        .data$ald_sector == "steel" ~ "Steel",
        TRUE ~ .data$ald_sector
      ),
      # TODO: this should be extracted into mapping file
      technology = dplyr::case_when(
        .data$technology == "coalcap" ~ "CoalCap",
        .data$technology == "gascap" ~ "GasCap",
        .data$technology == "renewablescap" ~ "RenewablesCap",
        .data$technology == "nuclearcap" ~ "NuclearCap",
        .data$technology == "hydrocap" ~ "HydroCap",
        .data$technology == "oilcap" ~ "OilCap",
        .data$technology == "oil" ~ "Oil",
        .data$technology == "gas" ~ "Gas",
        .data$technology == "coal" ~ "Coal",
        .data$technology == "electric" ~ "Electric",
        .data$technology == "hybrid" ~ "Hybrid",
        .data$technology == "ice" ~ "ICE",
        TRUE ~ .data$technology
      )
    ) %>%
    # adjusting scenario column to hold source_scenario, to be compatible with PACTA
    # results for EQ and CB
    dplyr::mutate(scenario = paste(.data$scenario_source, .data$scenario, sep = "_")) %>%
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
