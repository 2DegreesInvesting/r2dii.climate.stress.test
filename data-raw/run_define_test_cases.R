devtools::load_all()

high_carbon_tech <- c("Oil", "Gas", "Coal", "CoalCap", "GasCap", "OilCap", "ICE")
investor_name_test <- "Meta Investor"
portfolio_name_test <- "Meta Portfolio"
asset_type_test <- "Equity"
portfolio_size <- 2000000

###----1 - Define Company trajectories

###----- standard case incl technology with increasing target----
# company 101 & company 102 have an increasing and a decreasing technology each
# they are misaligned in both technologies
# they have different market shares

###----- case incl technology with increasing target and (partially) aligned plans----
# companies 103, 104 and 105 have an increasing and a decreasing technology each
# company 103 is aligned in the high carbon technology, but misaligned in the low carbon technology
# company 104 is aligned in the low carbon technology, but misaligned in the high carbon technology
# company 105 is aligned in both technologies

###----- case incl technology with increasing target and zero start value----
# company 106 has 0 production of an increasing technology and no build out plans

###----- case incl technology with increasing target and production phase out----
# company 107 phases out an increasing technology

###----- case incl ONLY technology with increasing target and diff alignments----
# unsure about appropriate targets in this case... since only SMSP is used..
# company 108 has two misaligned increasing technologies
# company 108 has one aligned and one misaligned increasing technology
# company 110 has two aligned increasing technologies

###----- case incl ONLY technology with increasing target and diff alignments----
# unsure about appropriate targets in this case... since only SMSP is used..
# company 108 has two misaligned increasing technologies
# company 108 has one aligned and one misaligned increasing technology
# company 110 has two aligned increasing technologies

###----- case incl ONLY technology with decreasing target and diff alignments----
# this can only happen in COAL or O&G, because in Auto and Power, companies would
# be forced to build out low carbon technology regardless of start value
# company 111 has two misaligned decreasing technologies
# company 112 has one aligned and one misaligned decreasing technology
# company 113 has two aligned decreasing technologies


test_cases <- tibble::tibble(
  investor_name = investor_name_test,
  portfolio_name = portfolio_name_test,
  scenario_source = "WEO2019",
  scenario = rep.int(c("WEO2019_NPS", "WEO2019_SDS"), 156),
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
  plan_alloc_wt_tech_prod = plan_tech_prod * allocation_weight,
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
  scen_alloc_wt_tech_prod = scen_tech_prod * allocation_weight,
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
  dplyr::group_by(id, year, scenario, ald_sector) %>%
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
  dplyr::group_by(id, year, scenario, ald_sector) %>%
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

test_cases_long <- test_cases %>%
  dplyr::select(
    scenario_source,
    scenario,
    id,
    company_name,
    scenario_geography,
    equity_market,
    year,
    financial_sector,
    ald_sector,
    technology,
    plan_tech_prod,
    scen_tech_prod
  ) %>%
  tidyr::pivot_longer(
    cols = c(plan_tech_prod, scen_tech_prod),
    names_to = "metric",
    values_to = "production"
  ) %>%
  dplyr::mutate(
    metric = dplyr::if_else(
      .data$metric == "plan_tech_prod",
      "planned_production",
      .data$scenario
    )
  ) %>%
  dplyr::select(-c(.data$scenario_source, .data$scenario)) %>%
  dplyr::distinct_all() # remove duplicates introduced by pivoting


plot_example_company <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$production, color = .data$metric)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(.data$id ~ .data$ald_sector + .data$technology, scales = "free")
}

test_cases_long %>% plot_example_company()

###----2 - Define Portfolio Exposures
# we assume that all companies are valid inputs of the main asset type

test_portfolio_distribution <- test_cases %>%
  dplyr::filter(.data$year == min(.data$year, na.rm = TRUE)) %>%
  dplyr::distinct(.data$investor_name, .data$portfolio_name, .data$financial_sector, .data$id, .data$technology, .data$plan_carsten) %>%
  dplyr::group_by(.data$investor_name, .data$portfolio_name, .data$financial_sector) %>%
  dplyr::summarise(plan_carsten = sum(.data$plan_carsten, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    asset_type = asset_type_test,
    valid_input = TRUE,
    value = .data$plan_carsten * portfolio_size
  )

test_exposures <- tibble::tibble(
  investor_name = investor_name_test,
  portfolio_name = portfolio_name_test,
  asset_type = c(
    rep.int(asset_type_test, 10),
    asset_type_test,
    "Others",
    "Unclassifiable"
  ), # potential other types: "Bonds", "Loans", "Funds", "Other", "Unclassifiable"
  financial_sector = c(
    "Automotive",
    "Aviation",
    "Cement",
    "Coal",
    "HDV",
    "Oil&Gas",
    "Other",
    "Power",
    "Shipping",
    "Steel",
    "Other",
    "Power",
    "Unclassifiable"
  ),
  valid_input = c(
    rep.int(TRUE, 10),
    FALSE,
    TRUE,
    FALSE
  ),
  valid_value_usd = NA_real_,
  asset_value_usd = NA_real_,
  portfolio_value_usd = NA_real_,
  currency = "USD"
)

# left join the exposures so that the other sectors can be filled with 0 for
# demo purposes
test_exposures <- test_exposures %>%
  dplyr::left_join(
    test_portfolio_distribution,
    by = c("investor_name", "portfolio_name", "asset_type", "financial_sector", "valid_input")
  ) %>%
  dplyr::mutate(
    valid_value_usd = dplyr::if_else(!is.na(.data$value), .data$value, 0)
  ) %>%
  dplyr::select(-.data$value) %>%
  dplyr::group_by(.data$investor_name, .data$portfolio_name, .data$asset_type, .data$valid_input) %>%
  dplyr::mutate(asset_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.data$investor_name, .data$portfolio_name, .data$valid_input) %>%
  dplyr::mutate(portfolio_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    portfolio_value_usd = dplyr::if_else(
      asset_type == "Unclassifiable",
      portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
      .data$portfolio_value_usd
    ),
    asset_value_usd = dplyr::if_else(
      asset_type == "Unclassifiable",
      portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
      .data$asset_value_usd
    ),
    valid_value_usd = dplyr::if_else(
      asset_type == "Unclassifiable",
      portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
      .data$valid_value_usd
    )
  )
# verify that total exposure equals portfolio size
