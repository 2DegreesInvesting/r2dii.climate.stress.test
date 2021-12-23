devtools::load_all()

high_carbon_tech <- c("Oil", "Gas", "Coal", "CoalCap", "GasCap", "OilCap", "ICE")

###----- standard case incl technology with increasing target----
# company 111 & company 222 have an increasing and a decreasing technology each
# they are misaligned in both technologies
# they have different market shares
standard_case_increasing_target <- tibble::tibble(
  investor_name = "Meta Investor",
  portfolio_name = "Meta Investor",
  scenario_source = "WEO2019",
  scenario = rep.int(c("WEO2019_NPS", "WEO2019_SDS"), 24),
  allocation = "portfolio_weight",
  id = c(rep.int(111, 24), rep.int(222, 24)),
  company_name = c(rep.int("power company 111", 24), rep.int("power company 222", 24)),
  financial_sector = "Power",
  port_weight = 0.1,
  allocation_weight = 0.1,
  plan_br_dist_alloc_wt = 0.1,
  scen_br_dist_alloc_wt = 0.1,
  equity_market = "GlobalMarket",
  scenario_geography = "Global",
  year = rep.int(c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025), 4),
  ald_sector = "Power",
  technology = rep.int(c(rep.int("CoalCap", 12), rep.int("RenewablesCap", 12)), 2),
  plan_tech_prod = c(
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
    100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
    1000, 1000, 1100, 1100, 1200, 1200, 1300, 1300, 1400, 1400, 1500, 1500,
    100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105
  ),
  plan_alloc_wt_tech_prod = plan_tech_prod * allocation_weight,
  plan_carsten = NA_real_,
  plan_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2),
  scen_tech_prod = c(
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
    1000, 1000, 950, 900, 900, 800, 850, 700, 800, 600, 750, 500,
    100, 100, 150, 200, 200, 300, 250, 400, 300, 500, 350, 600
  ),
  scen_alloc_wt_tech_prod = scen_tech_prod * allocation_weight,
  scen_carsten = NA_real_,
  scen_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2), # not logical
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

standard_case_increasing_target <- standard_case_increasing_target %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE),
    plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
    plan_sec_emission_factor = weighted.mean(plan_emission_factor, w = plan_tech_prod),
    scen_sec_prod = sum(scen_tech_prod, na.rm = TRUE),
    scen_alloc_wt_sec_prod = sum(scen_alloc_wt_tech_prod, na.rm = TRUE),
    scen_sec_emission_factor = weighted.mean(scen_emission_factor, w = scen_tech_prod)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    plan_carsten = (plan_tech_prod / plan_sec_prod) * allocation_weight,
    scen_carsten = (scen_tech_prod / scen_sec_prod) * allocation_weight,
    plan_tech_share = plan_tech_prod / plan_sec_prod,
    scen_tech_share = scen_tech_prod / scen_sec_prod
  ) %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_carsten = sum(plan_carsten, na.rm = TRUE),
    scen_sec_carsten = sum(scen_carsten, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trajectory_deviation = (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) /
      scen_alloc_wt_tech_prod,
  ) %>%
  dplyr::mutate(
    trajectory_deviation = dplyr::case_when(
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod == 0 ~ 0,
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod != 0 ~ -1,
      TRUE ~ trajectory_deviation
    )
  ) %>%
  dplyr::mutate(
    trajectory_alignment = dplyr::if_else(
      technology %in% high_carbon_tech,
      -1 * trajectory_deviation,
      trajectory_deviation
    )
  )

###----- case incl technology with increasing target and (partially) aligned plans----
# companies 333, 444 and 555 have an increasing and a decreasing technology each
# company 333 is aligend in the high carbon technology, but misaligned in the low carbon technology
# company 444 is aligend in the low carbon technology, but misaligned in the high carbon technology
# company 555 is aligend in both technologies
case_increasing_target_aligned <- tibble::tibble(
  investor_name = "Meta Investor",
  portfolio_name = "Meta Investor",
  scenario_source = "WEO2019",
  scenario = rep.int(c("WEO2019_NPS", "WEO2019_SDS"), 36),
  allocation = "portfolio_weight",
  id = c(rep.int(333, 24), rep.int(444, 24), rep.int(555, 24)),
  company_name = c(rep.int("power company 333", 24), rep.int("power company 444", 24), rep.int("power company 555", 24)),
  financial_sector = "Power",
  port_weight = 0.1,
  allocation_weight = 0.1,
  plan_br_dist_alloc_wt = 0.1,
  scen_br_dist_alloc_wt = 0.1,
  equity_market = "GlobalMarket",
  scenario_geography = "Global",
  year = rep.int(c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025), 6),
  ald_sector = "Power",
  technology = rep.int(c(rep.int("CoalCap", 12), rep.int("RenewablesCap", 12)), 3),
  plan_tech_prod = c(
    100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
    100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105,
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
    100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50,
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150
  ),
  plan_alloc_wt_tech_prod = plan_tech_prod * allocation_weight,
  plan_carsten = NA_real_,
  plan_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 3),
  scen_tech_prod = c(
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150
  ),
  scen_alloc_wt_tech_prod = scen_tech_prod * allocation_weight,
  scen_carsten = NA_real_,
  scen_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 3), # not logical
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

case_increasing_target_aligned <- case_increasing_target_aligned %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE),
    plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
    plan_sec_emission_factor = weighted.mean(plan_emission_factor, w = plan_tech_prod),
    scen_sec_prod = sum(scen_tech_prod, na.rm = TRUE),
    scen_alloc_wt_sec_prod = sum(scen_alloc_wt_tech_prod, na.rm = TRUE),
    scen_sec_emission_factor = weighted.mean(scen_emission_factor, w = scen_tech_prod)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    plan_carsten = (plan_tech_prod / plan_sec_prod) * allocation_weight,
    scen_carsten = (scen_tech_prod / scen_sec_prod) * allocation_weight,
    plan_tech_share = plan_tech_prod / plan_sec_prod,
    scen_tech_share = scen_tech_prod / scen_sec_prod
  ) %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_carsten = sum(plan_carsten, na.rm = TRUE),
    scen_sec_carsten = sum(scen_carsten, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trajectory_deviation = (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) /
      scen_alloc_wt_tech_prod,
  ) %>%
  dplyr::mutate(
    trajectory_deviation = dplyr::case_when(
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod == 0 ~ 0,
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod != 0 ~ -1,
      TRUE ~ trajectory_deviation
    )
  ) %>%
  dplyr::mutate(
    trajectory_alignment = dplyr::if_else(
      technology %in% high_carbon_tech,
      -1 * trajectory_deviation,
      trajectory_deviation
    )
  )

###----- case incl technology with increasing target and zero start value----
# company 666 has 0 production of an increasing technology and no build out plans
case_increasing_target_start_zero <- tibble::tibble(
  investor_name = "Meta Investor",
  portfolio_name = "Meta Investor",
  scenario_source = "WEO2019",
  scenario = rep.int(c("WEO2019_NPS", "WEO2019_SDS"), 24),
  allocation = "portfolio_weight",
  id = c(rep.int(666, 24), rep.int(222, 24)),
  company_name = c(rep.int("power company 666", 24), rep.int("power company 222", 24)),
  financial_sector = "Power",
  port_weight = 0.1,
  allocation_weight = 0.1,
  plan_br_dist_alloc_wt = 0.1,
  scen_br_dist_alloc_wt = 0.1,
  equity_market = "GlobalMarket",
  scenario_geography = "Global",
  year = rep.int(c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025), 4),
  ald_sector = "Power",
  technology = rep.int(c(rep.int("CoalCap", 12), rep.int("RenewablesCap", 12)), 2),
  plan_tech_prod = c(
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1000, 1000, 1100, 1100, 1200, 1200, 1300, 1300, 1400, 1400, 1500, 1500,
    100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105
  ),
  plan_alloc_wt_tech_prod = plan_tech_prod * allocation_weight,
  plan_carsten = NA_real_,
  plan_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2),
  scen_tech_prod = c(
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    0, 0, 5, 10, 10, 20, 15, 30, 20, 40, 25, 50,
    1000, 1000, 950, 900, 900, 800, 850, 700, 800, 600, 750, 500,
    100, 100, 150, 200, 200, 300, 250, 400, 300, 500, 350, 600
  ),
  scen_alloc_wt_tech_prod = scen_tech_prod * allocation_weight,
  scen_carsten = NA_real_,
  scen_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2), # not logical
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

case_increasing_target_start_zero <- case_increasing_target_start_zero %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE),
    plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
    plan_sec_emission_factor = weighted.mean(plan_emission_factor, w = plan_tech_prod),
    scen_sec_prod = sum(scen_tech_prod, na.rm = TRUE),
    scen_alloc_wt_sec_prod = sum(scen_alloc_wt_tech_prod, na.rm = TRUE),
    scen_sec_emission_factor = weighted.mean(scen_emission_factor, w = scen_tech_prod)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    plan_carsten = (plan_tech_prod / plan_sec_prod) * allocation_weight,
    scen_carsten = (scen_tech_prod / scen_sec_prod) * allocation_weight,
    plan_tech_share = plan_tech_prod / plan_sec_prod,
    scen_tech_share = scen_tech_prod / scen_sec_prod
  ) %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_carsten = sum(plan_carsten, na.rm = TRUE),
    scen_sec_carsten = sum(scen_carsten, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trajectory_deviation = (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) /
      scen_alloc_wt_tech_prod,
  ) %>%
  dplyr::mutate(
    trajectory_deviation = dplyr::case_when(
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod == 0 ~ 0,
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod != 0 ~ -1,
      TRUE ~ trajectory_deviation
    )
  ) %>%
  dplyr::mutate(
    trajectory_alignment = dplyr::if_else(
      technology %in% high_carbon_tech,
      -1 * trajectory_deviation,
      trajectory_deviation
    )
  )


###----- case incl technology with increasing target and production phase out----
# company 777 phases out an increasing technology
case_increasing_target_phase_out <- tibble::tibble(
  investor_name = "Meta Investor",
  portfolio_name = "Meta Investor",
  scenario_source = "WEO2019",
  scenario = rep.int(c("WEO2019_NPS", "WEO2019_SDS"), 24),
  allocation = "portfolio_weight",
  id = c(rep.int(777, 24), rep.int(222, 24)),
  company_name = c(rep.int("power company 777", 24), rep.int("power company 222", 24)),
  financial_sector = "Power",
  port_weight = 0.1,
  allocation_weight = 0.1,
  plan_br_dist_alloc_wt = 0.1,
  scen_br_dist_alloc_wt = 0.1,
  equity_market = "GlobalMarket",
  scenario_geography = "Global",
  year = rep.int(c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025), 4),
  ald_sector = "Power",
  technology = rep.int(c(rep.int("CoalCap", 12), rep.int("NuclearCap", 12)), 2),
  plan_tech_prod = c(
    100, 100, 110, 110, 120, 120, 130, 130, 140, 140, 150, 150,
    100, 100, 70, 70, 30, 30, 0, 0, 0, 0, 0, 0,
    1000, 1000, 1100, 1100, 1200, 1200, 1300, 1300, 1400, 1400, 1500, 1500,
    100, 100, 101, 101, 102, 102, 103, 103, 104, 104, 105, 105
  ),
  plan_alloc_wt_tech_prod = plan_tech_prod * allocation_weight,
  plan_carsten = NA_real_,
  plan_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2),
  scen_tech_prod = c(
    100, 100, 95, 90, 90, 80, 85, 70, 80, 60, 75, 50,
    100, 100, 105, 110, 110, 120, 115, 130, 120, 140, 125, 150,
    1000, 1000, 950, 900, 900, 800, 850, 700, 800, 600, 750, 500,
    100, 100, 150, 200, 200, 300, 250, 400, 300, 500, 350, 600
  ),
  scen_alloc_wt_tech_prod = scen_tech_prod * allocation_weight,
  scen_carsten = NA_real_,
  scen_emission_factor = rep.int(c(rep.int(1, 12), rep.int(0, 12)), 2), # not logical
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

case_increasing_target_phase_out <- case_increasing_target_phase_out %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE),
    plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
    plan_sec_emission_factor = weighted.mean(plan_emission_factor, w = plan_tech_prod),
    scen_sec_prod = sum(scen_tech_prod, na.rm = TRUE),
    scen_alloc_wt_sec_prod = sum(scen_alloc_wt_tech_prod, na.rm = TRUE),
    scen_sec_emission_factor = weighted.mean(scen_emission_factor, w = scen_tech_prod)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    plan_carsten = (plan_tech_prod / plan_sec_prod) * allocation_weight,
    scen_carsten = (scen_tech_prod / scen_sec_prod) * allocation_weight,
    plan_tech_share = plan_tech_prod / plan_sec_prod,
    scen_tech_share = scen_tech_prod / scen_sec_prod
  ) %>%
  dplyr::group_by(id, scenario, ald_sector) %>%
  dplyr::mutate(
    plan_sec_carsten = sum(plan_carsten, na.rm = TRUE),
    scen_sec_carsten = sum(scen_carsten, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trajectory_deviation = (plan_alloc_wt_tech_prod - scen_alloc_wt_tech_prod) /
      scen_alloc_wt_tech_prod,
  ) %>%
  dplyr::mutate(
    trajectory_deviation = dplyr::case_when(
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod == 0 ~ 0,
      scen_alloc_wt_tech_prod == 0 & plan_alloc_wt_tech_prod != 0 ~ -1,
      TRUE ~ trajectory_deviation
    )
  ) %>%
  dplyr::mutate(
    trajectory_alignment = dplyr::if_else(
      technology %in% high_carbon_tech,
      -1 * trajectory_deviation,
      trajectory_deviation
    )
  )
