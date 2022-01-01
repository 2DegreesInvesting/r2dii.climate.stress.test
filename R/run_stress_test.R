#' Run stress testing for provided asset type.
#'
#' This function runs the transition risk stress test. It can be desirable to
#' understand sensitivities of the scenarios, in which case the user may pass a
#' vector of values to one (and only one) of the detail arguments. This will
#' result in running the analysis multiple times in a row with the argument
#' varied.
#' NOTE: argument `asset_type` and `term` cannot be iterated.
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
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `stress_test_arguments`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `stress_test_arguments`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `stress_test_arguments`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `stress_test_arguments`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `stress_test_arguments`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `stress_test_arguments`.
#' @param term Numeric. A coefficient that determines for which maturity the
#'   expected loss should be calculated in the credit risk section. For accepted
#'   range compare `stress_test_arguments`.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded. For accepted values compare
#'   `stress_test_arguments`.
#' @param use_company_terms Boolean, indicating if term values for individual
#'   companies are to be used. For accepted values compare
#'   `stress_test_arguments`. Note that currently this functionality is not
#'   available for asset_type bonds.
#' @param return_results Boolean, indicating if results shall be exported.
#' @return NULL
#' @export
run_stress_test <- function(asset_type,
                            input_path_project_specific,
                            input_path_project_agnostic,
                            output_path,
                            lgd_senior_claims = 0.45,
                            lgd_subordinated_claims = 0.75,
                            risk_free_rate = 0.02,
                            discount_rate = 0.07,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            term = 2,
                            company_exclusion = TRUE,
                            use_company_terms = FALSE,
                            return_results = FALSE) {
  cat("-- Running transition risk stress test. \n\n\n")

  cat("-- Validating input arguments. \n")

  validate_input_values(
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    term = term,
    company_exclusion = company_exclusion,
    use_company_terms = use_company_terms,
    asset_type = asset_type
  )

  validate_use_company_terms(
    asset_type = asset_type,
    use_company_terms = use_company_terms
  )

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()
  iter_var <- get_iter_var(args_list)
  args_list$output_path <- customise_output_path(args_list$output_path, iter_var)
  st_results_list <- run_stress_test_iteration(args_list, iter_var)

  result_names <- names(st_results_list[[1]])
  st_results <- result_names %>%
    purrr::map(function(tib) {
      purrr::map_dfr(st_results_list, `[[`, tib)
    }) %>%
    purrr::set_names(result_names)

  st_results_wrangled_and_checked <- wrangle_results(
    results_list = st_results,
    sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup]
  ) %>%
    check_results(
      sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup]
    ) %>%
    rename_results()

  if (return_results) {
    return(st_results_wrangled_and_checked)
  }

  write_stress_test_results(
    results_list = st_results_wrangled_and_checked,
    asset_type = asset_type,
    iter_var = iter_var,
    output_path = args_list$output_path
  )

  cat("-- Exported results to designated output path. \n")
}

#' Iterate over stress test runs
#'
#' @param n Numeric.
#' @param args_tibble  A tibble holding a set of params for
#'   `run_stress_test_imp` per row.
#'
#' @return List of stress test results.
run_stress_test_iteration <- function(args_list, iter_var) {
  args <- tibble::as_tibble(args_list) %>%
    dplyr::mutate(iter_var = .env$iter_var)

  n <- nrow(args)
  out <- as.list(logical(n))

  for (i in seq_len(n)) {
    args_row_i <- dplyr::slice(args, i)

    args_i <- args_row_i %>%
      dplyr::select(-.data$return_results) %>%
      as.list()

    arg_metadata <- args_row_i %>%
      dplyr::select(-dplyr::all_of(setup_vars_lookup)) %>%
      dplyr::rename_with(~ paste0(.x, "_arg"))

    out[[i]] <- args_i %>%
      run_stress_test_impl() %>%
      purrr::map(dplyr::bind_cols, data_y = arg_metadata)
  }

  return(out)
}

#' Run stress testing for provided asset type.
#'
#' Runs stress test per iteration.
#'
#' @inheritParams run_stress_test
#' @inheritParams write_stress_test_results
#'
#' @return A list of stress test results.
run_stress_test_impl <- function(args_list) {
  out <- args_list %>%
    # TODO: This should be done once, not one time per iteration
    read_input_data() %>%
    compute_results_loss_and_changes(args_list)

  return(out)
}

log_args <- function(args_list, log_path) {
  paste_write("\n\nIteration with parameter settings:", log_path = log_path)
  purrr::walk(names(args_list), function(name) {
    out <- paste(name, magrittr::extract2(args_list, name), sep = ": ")
    out <- paste_write(out, log_path = log_path)
    return(out)
  })
  paste_write("\n", log_path = log_path)

  return(invisible(args_list))
}

log_path <- function(args_list) {
  out <- file.path(
    args_list$output_path, paste0("log_file_", args_list$iter_var, ".txt")
  )
  return(out)
}

read_input_data <- function(args_list) {
  cat("-- Importing and preparing input data from designated input path. \n")

  # TODO: It's more transparent to access the elements explicitely
  list2env(args_list, envir = rlang::current_env())

  pacta_results <- input_path_project_specific %>%
    file.path(paste0(stringr::str_to_title(asset_type), "_results_", calculation_level_lookup, ".rda")) %>%
    read_pacta_results()

  start_year <- min(pacta_results$year, na.rm = TRUE)

  pacta_results <- pacta_results %>%
    process_pacta_results(
      start_year = start_year,
      end_year = end_year_lookup,
      time_horizon = time_horizon_lookup,
      scenario_geography_filter = scenario_geography_filter_lookup,
      scenarios_filter = scenarios_filter(),
      equity_market_filter = equity_market_filter_lookup,
      term = term,
      sectors = sectors_lookup,
      technologies = technologies_lookup,
      allocation_method = allocation_method_lookup
    )

  sector_exposures <- input_path_project_specific %>%
    file.path("overview_portfolio.rda") %>%
    read_sector_exposures() %>%
    process_sector_exposures(asset_type = asset_type)

  capacity_factors_power <- input_path_project_agnostic %>%
    file.path("prewrangled_capacity_factors_WEO_2020.csv") %>%
    read_capacity_factors() %>%
    process_capacity_factors_power(
      scenarios_filter = scenarios_filter(),
      scenario_geography_filter = scenario_geography_filter_lookup,
      technologies = technologies_lookup,
      start_year = start_year,
      end_year = end_year_lookup
    )

  excluded_companies <- input_path_project_agnostic %>%
    file.path("exclude-companies.csv") %>%
    read_excluded_companies() %>%
    process_excluded_companies(
      company_exclusion = company_exclusion,
      technologies = technologies_lookup
    )

  df_price <- input_path_project_agnostic %>%
    file.path(paste0("prices_data_", price_data_version_lookup, ".csv")) %>%
    read_price_data_old2() %>%
    process_df_price(
      technologies = technologies_lookup,
      sectors = sectors_lookup,
      start_year = start_year,
      end_year = end_year_lookup
    )

  scenario_data <- input_path_project_agnostic %>%
    file.path(paste0("Scenarios_AnalysisInput_", start_year, ".csv")) %>%
    read_scenario_data() %>%
    process_scenario_data(
      start_year = start_year,
      end_year = end_year_lookup,
      sectors = sectors_lookup,
      technologies = technologies_lookup,
      scenario_geography_filter = scenario_geography_filter_lookup,
      scenarios_filter = scenarios_filter()
    )

  financial_data <- input_path_project_agnostic %>%
    file.path("prewrangled_financial_data_stress_test.csv") %>%
    read_financial_data() %>%
    process_financial_data(asset_type = asset_type)

  if (asset_type == "loans") {
    financial_data <- financial_data %>%
      dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name))
  }

  # FIXME: Is this dead code?
  company_terms <- input_path_project_specific %>%
    file.path("company_terms.csv") %>%
    read_company_terms(use_company_terms)

  return(list(
    start_year = start_year,
    pacta_results = pacta_results,
    capacity_factors_power = capacity_factors_power,
    excluded_companies = excluded_companies,
    sector_exposures = sector_exposures,
    scenario_data = scenario_data,
    df_price = df_price,
    financial_data = financial_data
  ))
}

scenarios_filter <- function() {
  return(unique(c(baseline_scenario_lookup, shock_scenario_lookup)))
}

compute_results_loss_and_changes <- function(input_data_list, args_list) {
  # TODO: Move where?
  cat("-- Configuring analysis settings. \n")

  # TODO: It's more transparent to access the elements explicitely
  list2env(args_list, rlang::current_env())
  list2env(input_data_list, rlang::current_env())

  log_path <- log_path(args_list)
  log_args(args_list, log_path)

  report_company_drops(input_data_list, asset_type, log_path)

  transition_scenario <- start_year %>%
    generate_transition_shocks(
      end_of_analysis = end_year_lookup,
      shock_years = shock_year
    )

  # TODO: Move into calculate_exposure_by_technology_and_company?
  cat("-- Calculating market risk. \n")

  exposure_by_technology_and_company <- asset_type %>%
    calculate_exposure_by_technology_and_company(
      input_data_list = input_data_list,
      start_year = start_year,
      scenario_to_follow_ls = shock_scenario_lookup,
      log_path = log_path
    )

  annual_profits <- asset_type %>%
    calculate_annual_profits(
      input_data_list = input_data_list,
      scenario_to_follow_baseline = baseline_scenario_lookup,
      scenario_to_follow_ls = shock_scenario_lookup,
      transition_scenario = transition_scenario,
      start_year = start_year,
      end_year = end_year_lookup,
      time_horizon = time_horizon_lookup,
      discount_rate = discount_rate,
      log_path = log_path
    )
  port_aum <- calculate_aum(input_data_list$sector_exposures)
  results <- annual_profits %>%
    company_asset_value_at_risk(
      terminal_value = terminal_value_lookup,
      shock_scenario = transition_scenario,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      plan_carsten = exposure_by_technology_and_company,
      port_aum = port_aum,
      flat_multiplier = assign_flat_multiplier(asset_type = asset_type),
      exclusion = input_data_list$excluded_companies
    )

  # TODO: Move into calculate_pd_change_overall?
  cat("-- Calculating credit risk. \n\n\n")

  overall_pd_changes <- annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year_lookup,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  expected_loss <- overall_pd_changes %>%
    company_expected_loss(
      loss_given_default = assign_lgd(
        asset_type = asset_type,
        lgd_senior_claims = lgd_senior_claims,
        lgd_subordinated_claims = lgd_subordinated_claims
      ),
      exposure_at_default = exposure_by_technology_and_company,
      port_aum = port_aum
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient output from overall pd changes or related financial data inputs

  annual_pd_changes <- annual_profits %>%
    calculate_pd_change_annual(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year_lookup,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  return(
    list(
      results = results,
      expected_loss = expected_loss,
      annual_pd_changes = annual_pd_changes,
      overall_pd_changes = overall_pd_changes
    )
  )
}
