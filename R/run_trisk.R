#' Run stress testing for provided asset type.
#'
#' This function runs the transition risk stress test. It can be desirable to
#' understand sensitivities of the scenarios, in which case the user may pass a
#' vector of values to one (and only one) of the detail arguments. This will
#' result in running the analysis multiple times in a row with the argument
#' varied.
#' NOTE: if `return_results` is TRUE results will not be written to `output
#' path` but instead are returned.
#'
#' @param input_path String holding path to project agnostic data.
#' @param output_path String holding path to which output files are written.
#'   NOTE: Results and logs per run are saved to a subdirectory of output_path
#'   that will be generated automatically. The name of the subdirectory is the
#'   timestamp of the run of the analysis.
#' @param baseline_scenario Holds the name of the baseline scenario to be used
#'   in the stress test, for accepted value range check `stress_test_arguments`.
#' @param shock_scenario Holds the name of the shock scenario to be used in the
#'   stress test, for accepted value range check `stress_test_arguments`.
#' @param lgd Numeric, holding the loss given default for accepted value range
#'   check `stress_test_arguments`.
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
#' @param scenario_geography Character vector, indicating which geographical
#'   region(s) (concerning asset location) results shall be calculated for. For
#'   accepted values compare `stress_test_arguments`.
#' @param start_year Numeric, first year in the analysis used as the starting
#'   point from which production forecasts are compared against scenario targets.
#'   Must be available in the production data and indicates the first year of
#'   the scenario data.
#' @param carbon_price_model Character vector, indicating which NGFS model is used in regards to
#'   carbon prices. Default is no carbon tax.
#' @param market_passthrough Firm's ability to pass carbon tax onto the consumer
#' @param return_results Boolean, indicating if results shall be exported.
#' @param financial_stimulus Additional support for low carbon companies.
#'
#' @return NULL
#' @export
run_trisk <- function(input_path,
                      output_path,
                      baseline_scenario = "WEO2021_STEPS",
                      shock_scenario = "WEO2021_SDS",
                      lgd = 0.45,
                      risk_free_rate = 0.02,
                      discount_rate = 0.07,
                      growth_rate = 0.03,
                      div_netprofit_prop_coef = 1,
                      shock_year = 2030,
                      scenario_geography = "Global",
                      start_year = 2022,
                      carbon_price_model = "no_carbon_tax",
                      market_passthrough = 0,
                      financial_stimulus = 1,
                      return_results = FALSE) {
  cat("-- Running transition risk stress test. \n\n\n")

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()

  iter_var <- get_iter_var(args_list)

  cat("-- Validating input arguments. \n")

  # validate_input_values(
  #   baseline_scenario = baseline_scenario,
  #   shock_scenario = shock_scenario,
  #   scenario_geography = scenario_geography,
  #   lgd = lgd,
  #   risk_free_rate = risk_free_rate,
  #   discount_rate = discount_rate,
  #   growth_rate = growth_rate,
  #   div_netprofit_prop_coef = div_netprofit_prop_coef,
  #   shock_year = shock_year,
  #   carbon_price_model = carbon_price_model,
  #   market_passthrough = market_passthrough,
  #   financial_stimulus = financial_stimulus,
  #   risk_type = "trisk"
  # )

  scenario_type <- infer_scenario_type(
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario
  )


  args_list$output_path <- customise_output_path(
    output_path = args_list$output_path,
    iter_var = iter_var,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography,
    financial_stimulus = financial_stimulus,
    carbon_price_model = carbon_price_model,
    risk_type = "trisk"
  )

  args_list$end_year <- end_year_lookup(scenario_type = scenario_type)


  st_results_list <- run_stress_test_iteration(args_list)

  result_names <- names(st_results_list[[1]])
  st_results <- result_names %>%
    purrr::map(function(tib) {
      purrr::map_dfr(st_results_list, `[[`, tib)
    }) %>%
    purrr::set_names(result_names)

  st_results_wrangled_and_checked <- wrangle_results(
    results_list = st_results,
    sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup],
    risk_type = "trisk"
  ) %>%
    check_results(
      sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup],
      risk_type = "trisk"
    )

  if (return_results) {
    return(st_results_wrangled_and_checked)
  }

  write_stress_test_results(
    results_list = st_results_wrangled_and_checked,
    iter_var = iter_var,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography,
    carbon_price_model = carbon_price_model,
    financial_stimulus = financial_stimulus,
    risk_type = "trisk",
    output_path = args_list$output_path
  )

  cat("-- Exported results to designated output path. \n")
}

run_stress_test_iteration <- function(args_list) {
  run_stress_test_iteration_once <- function(arg_tibble_row) {
    arg_list_row <- arg_tibble_row %>%
      dplyr::select(-.data$return_results) %>%
      as.list()

    arg_tibble_row <- arg_tibble_row %>%
      dplyr::select(-dplyr::all_of(setup_vars_lookup)) %>%
      dplyr::rename_with(~ paste0(.x, "_arg"))

    st_result <- read_and_process_and_calc(arg_list_row) %>%
      purrr::map(dplyr::bind_cols, data_y = arg_tibble_row)

    return(st_result)
  }

  iter_var <- get_iter_var(args_list)
  args_tibble <- tibble::as_tibble(args_list) %>%
    dplyr::mutate(iter_var = .env$iter_var)

  out <- iteration_sequence(args_list) %>%
    purrr::map(~ dplyr::slice(args_tibble, .x)) %>%
    purrr::map(run_stress_test_iteration_once)

  return(out)
}

# Avoid R CMD check NOTE: "Undefined global functions or variables"
globalVariables(c(names(formals(run_trisk)), "iter_var", "end_year", "risk_type"))

read_and_process_and_calc <- function(args_list) {
  list2env(args_list, envir = rlang::current_env())

  if (financial_stimulus > 1) {
    log_path <- file.path(output_path, sprintf("log_file_%s_%s_%s_%s_%s.txt", iter_var, shock_scenario, scenario_geography, carbon_price_model, financial_stimulus))
  } else {
    log_path <- file.path(output_path, sprintf("log_file_%s_%s_%s_%s.txt", iter_var, shock_scenario, scenario_geography, carbon_price_model))
  }

  paste_write("\n\nIteration with parameter settings:", log_path = log_path)
  purrr::walk(names(args_list), function(name) {
    paste(name, magrittr::extract2(args_list, name), sep = ": ") %>%
      paste_write(log_path = log_path)
  })
  paste_write("\n", log_path = log_path)

  cat("-- Configuring analysis settings. \n")

  sectors_and_technologies_list <- infer_sectors_and_technologies(
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography
  )

  cat("-- Reading input data from designated input path. \n")

  data <- st_read_agnostic(input_path, start_year = start_year, sectors = sectors_and_technologies_list$sectors, risk_type = "trisk")

  cat("-- Processing input data. \n")

  processed <- data %>%
    st_process(
      scenario_geography = scenario_geography,
      baseline_scenario = baseline_scenario,
      shock_scenario = shock_scenario,
      sectors = sectors_and_technologies_list$sectors,
      technologies = sectors_and_technologies_list$technologies,
      start_year = start_year,
      carbon_price_model = carbon_price_model,
      log_path = log_path,
      end_year = end_year
    )

  input_data_list <- list(
    capacity_factors_power = processed$capacity_factors_power,
    scenario_data = processed$scenario_data,
    df_price = processed$df_price,
    financial_data = processed$financial_data,
    production_data = processed$production_data,
    carbon_data = processed$carbon_data
  )

  # TODO: this requires company company_id to work for all companies, i.e. using 2021Q4 PAMS data
  report_company_drops(
    data_list = input_data_list,
    log_path = log_path
  )

  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year,
    shock_year = shock_year
  )

  cat("-- Calculating production trajectory under trisk shock. \n")

  input_data_list$full_trajectory <- calculate_trisk_trajectory(
    input_data_list = input_data_list,
    baseline_scenario = baseline_scenario,
    target_scenario = shock_scenario,
    transition_scenario = transition_scenario,
    start_year = start_year,
    end_year = end_year,
    time_horizon = time_horizon_lookup,
    log_path = log_path
  )

  cat("-- Calculating net profits. \n")

  # calc net profits
  company_net_profits <- calculate_net_profits(input_data_list$full_trajectory,
    carbon_data = input_data_list$carbon_data,
    shock_year = shock_year,
    market_passthrough = market_passthrough,
    financial_stimulus = financial_stimulus
  )

  # calc discounted net profits
  company_annual_profits <- calculate_annual_profits(
    data = company_net_profits,
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    end_year = end_year,
    discount_rate = discount_rate,
    growth_rate = growth_rate,
    log_path = log_path
  )

  cat("-- Calculating market risk. \n")

  company_technology_npv <- company_annual_profits %>%
    company_technology_asset_value_at_risk(
      shock_scenario = transition_scenario,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      flat_multiplier = flat_multiplier_lookup,
      crispy = TRUE
    )

  cat("-- Calculating credit risk. \n\n\n")

  company_pd_changes_overall <- company_annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year,
      risk_free_interest_rate = risk_free_rate
    )

  return(
    list(
      company_pd_changes_overall = company_pd_changes_overall,
      company_trajectories = company_annual_profits,
      company_technology_npv = company_technology_npv
    )
  )
}

iteration_sequence <- function(args_list) {
  iter_var <- get_iter_var(args_list)
  if (identical(iter_var, "standard")) {
    return(1L)
  } else {
    return(seq_along(args_list[[iter_var]]))
  }
}
