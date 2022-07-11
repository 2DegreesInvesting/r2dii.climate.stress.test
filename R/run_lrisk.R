#' Run stress testing for provided asset type.
#'
#' This function runs the transition risk stress test. It can be desirable to
#' understand sensitivities of the scenarios, in which case the user may pass a
#' vector of values to one (and only one) of the detail arguments. This will
#' result in running the analysis multiple times in a row with the argument
#' varied.
#' NOTE: argument `asset_type` and `fallback_term` cannot be iterated.
#' NOTE: if `return_results` is TRUE results will not be written to `output
#' path` but instead are returned.
#'
#' @param asset_type String holding asset_type. For accepted values compare
#'   `stress_test_arguments`.
#' @param input_path_project_specific String holding path to project specific
#'   data.
#' @param input_path_project_agnostic String holding path to project agnostic
#'   data.
#' @param output_path String holding path to which output files are written.
#'   NOTE: Results and logs per run are saved to a subdirectory of output_path
#'   that will be generated automatically. The name of the subdirectory is the
#'   timestamp of the run of the analysis.
#' @param baseline_scenario Holds the name of the baseline scenario to be used
#'   in the stress test, for accepted value range check `stress_test_arguments`.
#' @param shock_scenario Holds the name of the shock scenario to be used in the
#'   stress test, for accepted value range check `stress_test_arguments`.
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `stress_test_arguments`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `stress_test_arguments`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `stress_test_arguments`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `stress_test_arguments`.
#' @param growth_rate Numeric, that holds the terminal growth rate of profits
#'   beyond the final year in the DCF. For accepted range compare
#'   `stress_test_arguments`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `stress_test_arguments`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `stress_test_arguments`.
#' @param fallback_term Numeric. A coefficient that determines for which
#'   maturity the expected loss should be calculated in the credit risk section
#'   in case no company level term data are provided via `use_company_terms`.
#'   For accepted range compare `stress_test_arguments`.
#' @param scenario_geography Character vector, indicating which geographical
#'   region(s) (concerning asset location) results shall be calculated for. For
#'   accepted values compare `stress_test_arguments`.
#' @param use_company_terms Boolean, indicating if term values for individual
#'   companies are to be used. For accepted values compare
#'   `stress_test_arguments`. Note that currently this functionality is not
#'   available for asset_type bonds.
#' @param chance_within_target bla
#' @param settlement_factor bla
#' @param percentage_in_terminal_value bla
#' @param percentage_rev_transition_sectors bla
#' @param net_profit_margin bla
#' @param reset_post_settlement bla
#' @param exp_share_damages_paid bla
#' @param scc bla
#' @param timeframe_emissions_overshoot bla
#' @param return_results Boolean, indicating if results shall be exported.
#' @return NULL
#' @export
run_lrisk <- function(asset_type,
                            input_path_project_specific,
                            input_path_project_agnostic,
                            output_path,
                            baseline_scenario = "WEO2020_SPS",
                            shock_scenario = "WEO2020_SDS",
                            lgd_senior_claims = 0.45,
                            lgd_subordinated_claims = 0.75,
                            risk_free_rate = 0.02,
                            discount_rate = 0.07,
                            growth_rate = 0.03,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            fallback_term = 2,
                            scenario_geography = "Global",
                            use_company_terms = FALSE,
                            chance_within_target = 0.66,
                            settlement_factor = 1,
                            percentage_in_terminal_value = 0.1,
                            percentage_rev_transition_sectors = 1,
                            net_profit_margin = 0.1,
                            reset_post_settlement = "start",
                            exp_share_damages_paid = 0.027,
                            scc = 40L,
                      # TODO: can this ever deviate from the forward looking horizon?
                            timeframe_emissions_overshoot = 5L,
                            return_results = FALSE) {

  # browser()

  cat("-- Running litigation risk stress test. \n\n\n")

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()

  iter_var <- get_iter_var(args_list)

  cat("-- Validating input arguments. \n")

  validate_input_values(
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography,
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    growth_rate = growth_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    fallback_term = fallback_term,
    use_company_terms = use_company_terms,
    asset_type = asset_type
  )

  args_list$output_path <- customise_output_path(
    output_path = args_list$output_path,
    iter_var = iter_var
  )

  #---- set params
  log_path <- file.path(output_path, paste0("log_file_", iter_var, ".txt"))

  # cfg_litigation_params <- config::get(file = "params_litigation_risk.yml")

  # ADO 1540 - set variables for reading company level PACTA results
  investor_name <- investor_name_placeholder
  portfolio_name <- investor_name_placeholder
  flat_multiplier <- assign_flat_multiplier(asset_type = asset_type)
  # TODO: currency needed?
  target_currency <- target_currency_lookup

  # ADO 1540 - use for filters
  horizon <- time_horizon_lookup

  scenario_geography_filter <- scenario_geography
  # scenario
  baseline_scenario = baseline_scenario
  shock_scenario = shock_scenario


  allocation_method <- allocation_method_lookup
  equity_market_filter <- equity_market_filter_lookup

  # TODO: use as in trisk
  # sectors <- cfg_litigation_params$large_universe_filter$sector_filter
  # technologies <- cfg_litigation_params$lists$technology_list

  sectors_and_technologies_list <- infer_sectors_and_technologies(
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography
  )

  years_to_litigation_event <- years_to_litigation_event_lookup

  #---- read data

  data <- st_read_specific(
    input_path_project_specific,
    asset_type = asset_type,
    use_company_terms = use_company_terms
  )
  start_year <- get_start_year(data)
  data <- append(
    data, st_read_agnostic(input_path_project_agnostic, start_year = start_year, sectors = sectors_and_technologies_list$sectors)
  )

  # TODO: this should use the shock_year argument instead
  year_litigation_event <- start_year + years_to_litigation_event

  litigation_risk_scenarios <- litigation_risk_scenarios_lookup

  cat("-- Processing input data. \n")

  processed <- data %>%
    st_process(
      asset_type = asset_type,
      fallback_term = fallback_term,
      scenario_geography = scenario_geography,
      baseline_scenario = baseline_scenario,
      shock_scenario = shock_scenario,
      sectors = sectors_and_technologies_list$sectors,
      technologies = sectors_and_technologies_list$technologies,
      log_path = log_path
    )

  input_data_list <- list(
    pacta_results = processed$pacta_results,
    capacity_factors_power = processed$capacity_factors_power,
    sector_exposures = processed$sector_exposures,
    scenario_data = processed$scenario_data,
    df_price = processed$df_price,
    financial_data = processed$financial_data
  )

  if (asset_type == "loans") {
    input_data_list$financial_data <- input_data_list$financial_data %>%
      dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name))
  }

  report_company_drops(
    data_list = input_data_list,
    asset_type = asset_type,
    log_path = log_path
  )

  # browser()

  port_aum <- calculate_aum(input_data_list$sector_exposures)

  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year_lookup,
    shock_year = shock_year
  )

  litigation_scenario <- tibble::tribble(
    ~litigation_scenario, ~model, ~exp_share_damages_paid, ~scc, ~timeframe_emissions_overshoot,
    glue::glue("SCC_{year_litigation_event}"),  "SCC",  exp_share_damages_paid,  exp_share_damages_paid,  timeframe_emissions_overshoot
  )

  cat("-- Calculating market risk. \n")

  # disentagle
  # company_annual_profits <- calculate_annual_profits(
  #   asset_type = asset_type,
  #   input_data_list = input_data_list,
  #   scenario_to_follow_baseline = baseline_scenario,
  #   scenario_to_follow_shock = shock_scenario,
  #   transition_scenario = transition_scenario,
  #   start_year = start_year,
  #   end_year = end_year_lookup,
  #   time_horizon = time_horizon_lookup,
  #   discount_rate = discount_rate,
  #   growth_rate = growth_rate,
  #   log_path = log_path
  # )

  # TODO: decide if a slow change in price trajectory is needed...
  # price_data <- input_data_list$df_price %>%
  #   calc_scenario_prices(
  #     baseline_scenario = baseline_scenario,
  #     shock_scenario = shock_scenario,
  #     transition_scenario = transition_scenario,
  #     start_year = start_year
  #   )
  # For now, we assume that we just have the standard prices which are renamed to be able to use functions
  price_data <- input_data_list$df_price %>%
    dplyr::rename(
      Baseline_price = !!rlang::sym(glue::glue("price_{baseline_scenario}")),
      late_sudden_price = !!rlang::sym(glue::glue("price_{shock_scenario}"))
    )
browser()
  # setting emission_factors = TRUE will make the function extend the EF targets
  # by applying the TMSR to the initial EF value (current solution in PACTA)
  # this should at some point be replaced with a proper SDA function for targets
  # for emission factors, but this is not yet implemented.
  extended_pacta_results <- input_data_list$pacta_results %>%
    extend_scenario_trajectory(
      scenario_data = input_data_list$scenario_data,
      start_analysis = start_year,
      end_analysis = end_year_lookup,
      time_frame = time_horizon_lookup,
      baseline_scenario = baseline_scenario,
      target_scenario = shock_scenario,
      emission_factors = TRUE
    ) %>%
    set_baseline_trajectory(
      scenario_to_follow_baseline = tolower(baseline_scenario)
    ) %>%
    # TODO: what should be the mechanism for the late_sudden/shock scenario?
    set_ls_trajectory(
      scenario_to_follow_ls = tolower(shock_scenario),
      shock_scenario = transition_scenario,
      scenario_to_follow_ls_aligned = tolower(shock_scenario),
      start_year = start_year,
      end_year = end_year_lookup,
      analysis_time_frame = time_horizon_lookup,
      log_path = log_path
    )

  # TODO: Calculate costs not just as diff to production trajectory, but add the SCC penalty


}
