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
                               investor_name = "investor_name",
                               portfolio_name = "portfolio_name",
                               credit = loan_share_credit_type) {
  data_has_required_cols <- all(c(
    "sector",
    "technology",
    "year",
    "region",
    "scenario_source",
    # TODO: is id_2dii required? corresponds to id in PACTA_analysis 1-to-1?
    # "id_2dii",
    "name_ald",
    "metric",
    "production_weighted",
    "production_unweighted",
    "technology_share",
    "loan_share_outstanding",
    "loan_share_credit_limit"
  ) %in% colnames(data))

  stopifnot(data_has_required_cols)

  credit_allowed <- credit %in% c(
    "loan_share_outstanding",
    "loan_share_credit_limit"
  )
  stopifnot(credit_allowed)

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

  results_loanbook <- data %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = investor_name,
      allocation = "portfolio_weight", # TODO: This should be an input and take whatever the workflow passes
      equity_market = "GlobalMarket", # TODO: This should be an input and take whatever the workflow passes
      credit_choice = !!rlang::sym(credit),
      plan_carsten = .data$technology_share * .data$credit_choice,
      plan_tech_prod = .data$production_unweighted,
      scen_tech_prod = .data$production_unweighted,
      plan_alloc_wt_tech_prod = .data$production_weighted,
      scen_alloc_wt_tech_prod = .data$production_weighted
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
        .data$production_weighted, .data$production_unweighted,
        .data$technology_share
      )
    ) %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::mutate(
      scenario_geography = paste0(toupper(substr(.data$scenario_geography, 1, 1)), substr(.data$scenario_geography, 2, nchar(.data$scenario_geography))),
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      plan_sec_carsten = sum(.data$plan_carsten, na.rm = TRUE),
      plan_alloc_wt_sec_prod = sum(.data$plan_alloc_wt_tech_prod, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      !!!rlang::syms(group_vars),
      .data$plan_tech_prod, .data$plan_alloc_wt_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$scen_alloc_wt_tech_prod, .data$plan_sec_prod, .data$plan_alloc_wt_sec_prod, .data$plan_sec_carsten
    )

  plan <- results_loanbook %>%
    dplyr::filter(.data$scenario == "projected") %>%
    dplyr::select(-c(.data$scen_tech_prod, .data$scen_alloc_wt_tech_prod))

  scen <- results_loanbook %>%
    dplyr::filter(.data$scenario %in% c("target_b2ds", "target_cps", "target_rts", "target_sps", "target_steps", "target_2ds", "target_sds")) %>% # TODO: pass corporate_economy and filter in workflow?
    dplyr::select(
      -c(
        .data$plan_tech_prod, .data$plan_alloc_wt_tech_prod, .data$plan_carsten,
        .data$plan_sec_prod, .data$plan_alloc_wt_sec_prod, .data$plan_sec_carsten
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
      .data$plan_tech_prod, .data$plan_alloc_wt_tech_prod, .data$plan_carsten, .data$scen_tech_prod,
      .data$scen_alloc_wt_tech_prod, .data$plan_sec_prod, .data$plan_alloc_wt_sec_prod, .data$plan_sec_carsten
    ) %>%
    dplyr::mutate(
      # TODO: validate scenario mapping
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
      ald_sector = dplyr::case_when(
        .data$ald_sector == "power" ~ "Power",
        .data$ald_sector == "oil and gas" ~ "Oil&Gas",
        .data$ald_sector == "coal" ~ "Coal",
        .data$ald_sector == "automotive" ~ "Automotive",
        .data$ald_sector == "steel" ~ "Steel",
        TRUE ~ .data$ald_sector
      ),
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
    dplyr::select(-.data$scenario_source)

  output_has_required_cols <- all(c(
    "investor_name",
    "portfolio_name",
    "scenario",
    "allocation",
    # "id",
    "company_name",
    "equity_market",
    "scenario_geography",
    "year",
    "ald_sector",
    "technology",
    "plan_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten",
    "scen_tech_prod",
    "scen_alloc_wt_tech_prod",
    "plan_sec_prod",
    "plan_alloc_wt_sec_prod",
    "plan_sec_carsten"
  ) %in% colnames(results_loanbook))

  stopifnot(output_has_required_cols)

  return(results_loanbook)
}
