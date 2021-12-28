#' Generates a toy PACTA style portfolio with example companies
#'
#' @description
#' Does not take any inputs. Generates a PACTA style portfolio that contains
#' companies representing stylized cases of interest for the transition risk
#' stress test.
#' @family example case functions
#' @return data frame
#' @export
generate_test_companies <- function() {
  test_cases <- tibble::tibble(
    investor_name = .env$investor_name_test,
    portfolio_name = .env$portfolio_name_test,
    scenario_source = .env$scenario_source_test,
    scenario = rep.int(.env$scenario_pair_test, 156),
    allocation = "portfolio_weight",
    id = c(
      rep.int(101, 24),
      rep.int(102, 24),
      rep.int(103, 24),
      rep.int(104, 24),
      rep.int(105, 24),
      rep.int(106, 24),
      rep.int(107, 24),
      rep.int(108, 24),
      rep.int(109, 24),
      rep.int(110, 24),
      rep.int(111, 24),
      rep.int(112, 24),
      rep.int(113, 24)
    ),
    company_name = c(
      rep.int("power company 101", 24),
      rep.int("power company 102", 24),
      rep.int("power company 103", 24),
      rep.int("power company 104", 24),
      rep.int("power company 105", 24),
      rep.int("power company 106", 24),
      rep.int("power company 107", 24),
      rep.int("power company 108", 24),
      rep.int("power company 109", 24),
      rep.int("power company 110", 24),
      rep.int("power company 111", 24),
      rep.int("power company 112", 24),
      rep.int("power company 113", 24)
    ),
    financial_sector = c(rep.int("Power", 240), rep.int("Oil&Gas", 72)),
    port_weight = 0.05,
    allocation_weight = 0.05,
    plan_br_dist_alloc_wt = 0.05,
    scen_br_dist_alloc_wt = 0.05,
    equity_market = "GlobalMarket",
    scenario_geography = "Global",
    year = rep.int(c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025), 26),
    ald_sector = c(rep.int("Power", 240), rep.int("Oil&Gas", 72)),
    technology = c(
      rep.int(c(rep.int("CoalCap", 12), rep.int("RenewablesCap", 12)), 6),
      rep.int(c(rep.int("CoalCap", 12), rep.int("NuclearCap", 12)), 1),
      rep.int(c(rep.int("RenewablesCap", 12), rep.int("NuclearCap", 12)), 3),
      rep.int(c(rep.int("Oil", 12), rep.int("Gas", 12)), 3)
    ),
    plan_tech_prod = c(
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      1000, 1000, 1100, 1100, 1200, 1200, 1300, 1300, 1400, 1400, 1500, 1500,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 70, 70, 30, 30, 0, 0, 0, 0, 0, 0,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
      100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
      100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
      100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
      100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50
    ),
    plan_alloc_wt_tech_prod = .data$plan_tech_prod * .data$allocation_weight,
    plan_carsten = NA_real_,
    plan_emission_factor = c(
      rep.int(c(rep.int(1, 12), rep.int(0, 12)), 7),
      rep.int(rep.int(0, 24), 3),
      rep.int(rep.int(1, 24), 3)
    ),
    scen_tech_prod = c(
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      1000, 1000, 950, 900, 900, 800, 850, 700, 800, 600, 750, 500,
      100, 100, 150, 200, 200, 300, 250, 400, 300, 500, 350, 600,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      0, 0, 5, 10, 10, 20, 15, 30, 20, 40, 25, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
      100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50
    ),
    scen_alloc_wt_tech_prod = .data$scen_tech_prod * .data$allocation_weight,
    scen_carsten = NA_real_,
    scen_emission_factor = c(
      rep.int(c(rep.int(1, 12), rep.int(0, 12)), 7),
      rep.int(rep.int(0, 24), 3),
      rep.int(rep.int(1, 24), 3)
    ), # not logical
    plan_sec_prod = NA_real_,
    plan_alloc_wt_sec_prod = NA_real_,
    plan_sec_carsten = NA_real_,
    plan_sec_emission_factor = NA_real_,
    scen_sec_prod = NA_real_,
    scen_alloc_wt_sec_prod = NA_real_,
    scen_sec_carsten = NA_real_,
    scen_sec_emissions_factor = NA_real_,
    plan_tech_share = NA_real_,
    scen_tech_share = NA_real_,
    trajectory_deviation = NA_real_,
    trajectory_alignment = NA_real_
  )

  test_cases <- test_cases %>%
    dplyr::group_by(.data$id, .data$year, .data$scenario, .data$ald_sector) %>%
    dplyr::mutate(
      plan_sec_prod = sum(.data$plan_tech_prod, na.rm = TRUE),
      plan_alloc_wt_sec_prod = sum(.data$plan_alloc_wt_tech_prod, na.rm = TRUE),
      plan_sec_emission_factor = weighted.mean(.data$plan_emission_factor, w = .data$plan_tech_prod),
      scen_sec_prod = sum(.data$scen_tech_prod, na.rm = TRUE),
      scen_alloc_wt_sec_prod = sum(.data$scen_alloc_wt_tech_prod, na.rm = TRUE),
      scen_sec_emission_factor = weighted.mean(.data$scen_emission_factor, w = .data$scen_tech_prod)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      plan_carsten = (.data$plan_tech_prod / .data$plan_sec_prod) * .data$allocation_weight,
      scen_carsten = (.data$scen_tech_prod / .data$scen_sec_prod) * .data$allocation_weight,
      plan_tech_share = .data$plan_tech_prod / .data$plan_sec_prod,
      scen_tech_share = .data$scen_tech_prod / .data$scen_sec_prod
    ) %>%
    dplyr::group_by(.data$id, .data$year, .data$scenario, .data$ald_sector) %>%
    dplyr::mutate(
      plan_sec_carsten = sum(.data$plan_carsten, na.rm = TRUE),
      scen_sec_carsten = sum(.data$scen_carsten, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      trajectory_deviation = (.data$plan_alloc_wt_tech_prod - .data$scen_alloc_wt_tech_prod) /
        .data$scen_alloc_wt_tech_prod,
    ) %>%
    dplyr::mutate(
      trajectory_deviation = dplyr::case_when(
        .data$scen_alloc_wt_tech_prod == 0 & .data$plan_alloc_wt_tech_prod == 0 ~ 0,
        .data$scen_alloc_wt_tech_prod == 0 & .data$plan_alloc_wt_tech_prod != 0 ~ -1,
        TRUE ~ .data$trajectory_deviation
      )
    ) %>%
    dplyr::mutate(
      trajectory_alignment = dplyr::if_else(
        .data$technology %in% .env$high_carbon_tech,
        -1 * .data$trajectory_deviation,
        .data$trajectory_deviation
      )
    )
}
