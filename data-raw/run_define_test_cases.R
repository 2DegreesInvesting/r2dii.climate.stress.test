devtools::load_all()

high_carbon_tech <- c("Oil", "Gas", "Coal", "CoalCap", "GasCap", "OilCap", "ICE")
investor_name_test <- "Meta Investor"
portfolio_name_test <- "Meta Portfolio"
scenario_source_test <- "WEO2019"
scenario_pair_test <- c("WEO2019_NPS", "WEO2019_SDS")
asset_type_test <- "Equity"
portfolio_size <- 2000000

### ----1 - Define Company trajectories

### ----- standard case incl technology with increasing target----
# company 101 & company 102 have an increasing and a decreasing technology each
# they are misaligned in both technologies
# they have different market shares

### ----- case incl technology with increasing target and (partially) aligned plans----
# companies 103, 104 and 105 have an increasing and a decreasing technology each
# company 103 is aligned in the high carbon technology, but misaligned in the low carbon technology
# company 104 is aligned in the low carbon technology, but misaligned in the high carbon technology
# company 105 is aligned in both technologies

### ----- case incl technology with increasing target and zero start value----
# company 106 has 0 production of an increasing technology and no build out plans

### ----- case incl technology with increasing target and production phase out----
# company 107 phases out an increasing technology

### ----- case incl ONLY technology with increasing target and diff alignments----
# unsure about appropriate targets in this case... since only SMSP is used..
# company 108 has two misaligned increasing technologies
# company 108 has one aligned and one misaligned increasing technology
# company 110 has two aligned increasing technologies

### ----- case incl ONLY technology with increasing target and diff alignments----
# unsure about appropriate targets in this case... since only SMSP is used..
# company 108 has two misaligned increasing technologies
# company 108 has one aligned and one misaligned increasing technology
# company 110 has two aligned increasing technologies

### ----- case incl ONLY technology with decreasing target and diff alignments----
# this can only happen in COAL or O&G, because in Auto and Power, companies would
# be forced to build out low carbon technology regardless of start value
# company 111 has two misaligned decreasing technologies
# company 112 has one aligned and one misaligned decreasing technology
# company 113 has two aligned decreasing technologies

# TODO: scenario paths should match the toy scenario data
test_cases <- generate_test_companies()

# generate QA graph for companies
test_cases_long <- test_cases %>%
  dplyr::select(
    .data$scenario_source,
    .data$scenario,
    .data$id,
    .data$company_name,
    .data$scenario_geography,
    .data$equity_market,
    .data$year,
    .data$financial_sector,
    .data$ald_sector,
    .data$technology,
    .data$plan_tech_prod,
    .data$scen_tech_prod
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

plot_example_company(test_cases_long)


### ----2 - Define Portfolio Exposures
# we assume that all companies are valid inputs of the main asset type
test_exposure <- generate_test_exposure(test_cases)
# TODO: verify that total exposure equals portfolio size

### ----3 - Define toy scenario data
# assumptions...
test_scenarios <- generate_test_scenarios()

# generate QA graph for scenarios
plot_example_scenario <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$fair_share_perc, color = .data$scenario)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$ald_sector + .data$technology)
}

plot_example_scenario(test_scenarios)


### ----4 - Define toy capacity factors data
# assumptions...
test_capacity_factors <- generate_test_capacity_factors()

### ----5 - Define toy price data (wide and long)
# assumptions...
test_price_data_wide <- generate_price_data_wide()
test_price_data <- generate_price_data()

### ----6 - Define toy financial data
# assumptions...
test_financial_data <- generate_test_financial_data(test_cases)

### ----7 - Define toy company exclusion data
# assumptions...
test_exclude_companies_data <- generate_test_exclude_companies_data()
