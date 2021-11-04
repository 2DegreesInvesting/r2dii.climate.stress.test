#' Run stress testing for provided asset type.
#'
#' @param asset_type String holding asset_type, for allowed value compare
#'   `asset_types_lookup`.
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `lgd_senior_claims_range_lookup`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `lgd_subordinated_claims_range_lookup`.
#' @param terminal_value Numeric. A ratio to determine the share of the
#'   discounted value used in the terminal value calculation beyond the
#'   projected time frame. For accepted range compare `terminal_value_range_lookup`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `risk_free_rate_range_lookup`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `discount_rate_range_lookup`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `div_netprofit_prop_coef_range_lookup`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `shock_year_range_lookup`.
#' @param term Numeric. A coefficient that determines for which maturity the
#'   expected loss should be calculated in the credit risk section. For accepted
#'   range compare `term_range_lookup`.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded.
#' @return NULL
#' @export
run_stress_test <- function(asset_type,
                            lgd_senior_claims = 0.45,
                            lgd_subordinated_claims = 0.75,
                            terminal_value = 0,
                            risk_free_rate = 0.02,
                            discount_rate = 0.02,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            term = 2,
                            company_exclusion = TRUE) {

  cat("-- Validating input arguments. \n")

  validate_input_values(
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    terminal_value = terminal_value,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    term = term,
    company_exclusion = company_exclusion,
    asset_type = asset_type
  )

  cat("-- Configuring analysis settings. \n")

  scenario_to_follow_baseline <- baseline_scenario_lookup
  scenario_to_follow_ls <- shock_scenario_lookup
  calculation_level <- calculation_level_lookup
  end_year <- end_year_lookup
  flat_multiplier <- assign_flat_multiplier(asset_type = asset_type)
  lgd <- assign_lgd(
    asset_type = asset_type, lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims
  )
  cfg <- config::get(file = file.path(get_st_data_path("ST_PROJECT_FOLDER"), "inputs", "AnalysisParameters.yml"))
  start_year <- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <- cfg$AnalysisPeriod$Years.Horizon
  scenario_geography_filter <- "Global"
  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls
    )
  )

  cat("-- Importing and preparing input data from designated input path. \n")

  project_specific_data_list <- read_and_prepare_project_specific_data(
    asset_type = asset_type,
    calculation_level = calculation_level,
    start_year = start_year,
    time_horizon = time_horizon,
    scenario_geography_filter = scenario_geography_filter,
    scenarios_filter = scenarios_filter,
    equity_market_filter = cfg$Lists$Equity.Market.List,
    term = term
  )

  project_agnostic_data_list <- read_and_prepare_project_agnostic_data(
    start_year = start_year,
    end_year = end_year,
    company_exclusion = company_exclusion,
    scenario_geography_filter = scenario_geography_filter,
    asset_type = asset_type
  )

  input_data_list <- c(project_specific_data_list, project_agnostic_data_list) %>%
    check_and_filter_data(
      start_year = start_year,
      end_year = end_year,
      scenarios_filter = scenarios_filter,
      scenario_geography_filter = scenario_geography_filter
    )

  if (asset_type == "loans") {
    input_data_list$financial_data <- input_data_list$financial_data %>%
      dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name))
  }

  check_scenario_availability(
    portfolio = input_data_list$pacta_results,
    scen_data = input_data_list$scenario_data,
    scenarios = scenarios_filter
  )

  # TODO: validate
  port_aum <- calculate_aum(input_data_list$sector_exposures)
  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year,
    shock_years = shock_year
  )

  cat("-- Calculating market risk. \n")

  annual_profits <- calculate_annual_profits(
    asset_type = asset_type,
    input_data_list = input_data_list,
    scenario_to_follow_baseline = scenario_to_follow_baseline,
    scenario_to_follow_ls = scenario_to_follow_ls,
    transition_scenario = transition_scenario,
    start_year = start_year,
    end_year = end_year,
    time_horizon = time_horizon,
    discount_rate = discount_rate
  )

  exposure_by_technology_and_company <- calculate_exposure_by_technology_and_company(
    asset_type = asset_type,
    input_data_list = input_data_list,
    start_year = start_year,
    scenario_to_follow_ls = scenario_to_follow_ls
  )

  results <- company_asset_value_at_risk(
    data = annual_profits,
    terminal_value = terminal_value,
    shock_scenario = transition_scenario,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    plan_carsten = exposure_by_technology_and_company,
    port_aum = port_aum,
    flat_multiplier = flat_multiplier,
    exclusion = input_data_list$excluded_companies
  )

  cat("-- Calculating credit risk. \n")

  overall_pd_changes <- annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  expected_loss <- company_expected_loss(
    data = overall_pd_changes,
    loss_given_default = lgd,
    exposure_at_default = exposure_by_technology_and_company,
    # TODO: what to do with this? some sector level exposure for loanbook?
    port_aum = port_aum
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient output from overall pd changes or related financial data inputs

  annual_pd_changes <- calculate_pd_change_annual(
    data = annual_profits,
    shock_year = transition_scenario$year_of_shock,
    end_of_analysis = end_year,
    risk_free_interest_rate = risk_free_rate
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  cat("-- Exporting results to designated output path. \n")

  write_stress_test_results(
    results = results,
    expected_loss = expected_loss,
    annual_pd_changes = annual_pd_changes,
    overall_pd_changes = overall_pd_changes,
    asset_type = asset_type,
    calculation_level = calculation_level
  )

  cat("-- Exported results to designated output path. \n")
}
