# FIXME: use different way to access configs than assigning everything to globalEnv. Causes NOTE in R CMD check
set_project_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  project_report_name <<- cfg$reporting$project_report_name
  display_currency <<- cfg$reporting$display_currency
  currency_exchange_value <<- as.numeric(cfg$reporting$currency_exchange_value)

  financial_timestamp <<- cfg$parameters$timestamp
  dataprep_timestamp <<- cfg$parameters$dataprep_timestamp

  start_year <<- as.numeric(cfg$parameters$start_year)
  time_horizon <<- as.numeric(cfg$parameters$horizon_year)

  select_scenario <<- cfg$parameters$select_scenario
  scenario_auto <<- cfg$parameters$scenario_auto
  scenario_other <<- cfg$parameters$scenario_other
  scenario_shipping <<- cfg$parameters$scenario_shipping
  portfolio_allocation_method <<- cfg$parameters$portfolio_allocation_method
  scenario_geography <<- cfg$parameters$scenario_geography

  tech_roadmap_sectors <<- cfg$sectors$tech_roadmap_sectors
  pacta_sectors_not_analysed <<- cfg$sectors$pacta_sectors_not_analysed
  sector_list <<- c(tech_roadmap_sectors, pacta_sectors_not_analysed)

  scenario_sources_list <<- cfg$scenario_sources_list
  scenario_geographies_list <<- cfg$scenario_geography_list
  asset_types <<- cfg$asset_types
  equity_market_list <<- cfg$equity_market_list

  grouping_variables <<- cfg$grouping_variables

  green_techs <<- cfg$sectors$green_techs
  alignment_techs <<- cfg$sectors$alignment_techs

  shock_year <<- cfg$stress_test$shock_year


  # meta_investor_name <<- cfg$ComparisonBenchmarks$MetaInvestorName
  # meta_portfolio_name <<- cfg$ComparisonBenchmarks$MetaPortfolioName

  inc_meta_portfolio <<- cfg$ComparisonBenchmarks$CreateMetaPortfolio
  if (is.null(inc_meta_portfolio)) {
    inc_meta_portfolio <<- FALSE
  }

  has_map <<- cfg$methodology$has_map
  if (is.null(has_map)) {
    has_map <<- TRUE
    warning("Warning: has_map set to standard value (TRUE) as it is not defined in the parameter file")
  }

  has_sb <<- cfg$methodology$has_sb
  if (is.null(has_sb)) {
    has_sb <<- FALSE
    warning("Warning: has_sb set to standard value (FALSE) as it is not defined in the parameter file")
  }

  has_credit <<- cfg$methodology$has_credit
  if (is.null(has_credit)) {
    has_credit <<- FALSE
    warning("Warning: has_credit set to standard value (FALSE) as it is not defined in the parameter file")
  }

  has_revenue <<- cfg$methodology$has_revenue
  if (is.null(has_revenue)) {
    has_revenue <<- FALSE
    warning("Warning: has_revenue set to standard value (FALSE) as it is not defined in the parameter file")
  }

  inc_emission_factors <<- cfg$methodology$inc_emissionfactors
  if (is.null(inc_emission_factors)) {
    inc_emission_factors <<- FALSE
    warning("Warning: inc_emission_factors set to standard value (FALSE) as it is not defined in the parameter file")
  }

  inc_stresstest <<- cfg$methodology$inc_stresstest
  if (is.null(inc_stresstest)) {
    inc_stresstest <<- FALSE
    warning("Warning: inc_stresstest set to standard value (FALSE) as it is not defined in the parameter file")
  }
}
