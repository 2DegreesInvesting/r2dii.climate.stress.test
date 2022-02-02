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

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()

  iter_var <- get_iter_var(args_list)

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

  args_list$output_path <- customise_output_path(
    output_path = args_list$output_path,
    iter_var = iter_var
  )

  st_results_list <- run_stress_test_iteration(args_list)

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

run_stress_test_iteration <- function(args_list) {
  run_stress_test_iteration_once <- function(arg_tibble_row) {
    arg_list_row <- arg_tibble_row %>%
      dplyr::select(-.data$return_results) %>%
      as.list()

    arg_tibble_row <- arg_tibble_row %>%
      dplyr::select(-dplyr::all_of(setup_vars_lookup)) %>%
      dplyr::rename_with(~ paste0(.x, "_arg"))

    st_result <- read_and_process(arg_list_row) %>%
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
globalVariables(c(names(formals(run_stress_test)), "iter_var"))
read_and_process <- function(args_list) {
  list2env(args_list, envir = rlang::current_env())

  log_path <- file.path(output_path, paste0("log_file_", iter_var, ".txt"))

  paste_write("\n\nIteration with parameter settings:", log_path = log_path)
  purrr::walk(names(args_list), function(name) {
    paste(name, magrittr::extract2(args_list, name), sep = ": ") %>%
      paste_write(log_path = log_path)
  })
  paste_write("\n", log_path = log_path)

  cat("-- Configuring analysis settings. \n")

  flat_multiplier <- assign_flat_multiplier(asset_type = asset_type)
  lgd <- assign_lgd(
    asset_type = asset_type, lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims
  )

  cat("-- Reading input data from designated input path. \n")

  data <- st_read_specific(
    input_path_project_specific,
    asset_type = asset_type,
    use_company_terms = use_company_terms
  )
  start_year <- get_start_year(data)
  data <- append(
    data, st_read_agnostic(input_path_project_agnostic, start_year = start_year)
  )

  cat("-- Processing input data. \n")
  processed <- data %>%
    st_process(
      asset_type = asset_type,
      company_exclusion = company_exclusion,
      term = term
    )

  input_data_list <- list(
    pacta_results = processed$pacta_results,
    capacity_factors_power = processed$capacity_factors_power,
    excluded_companies = processed$excluded_companies,
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

  port_aum <- calculate_aum(input_data_list$sector_exposures)
  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year_lookup,
    shock_years = shock_year
  )

  cat("-- Calculating market risk. \n")

  annual_profits <- calculate_annual_profits(
    asset_type = asset_type,
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

  exposure_by_technology_and_company <- calculate_exposure_by_technology_and_company(
    asset_type = asset_type,
    input_data_list = input_data_list,
    start_year = start_year,
    time_horizon = time_horizon_lookup,
    scenario_to_follow_ls = shock_scenario_lookup,
    log_path = log_path
  )

  results <- company_asset_value_at_risk(
    data = annual_profits,
    terminal_value = terminal_value_lookup,
    shock_scenario = transition_scenario,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    plan_carsten = exposure_by_technology_and_company,
    port_aum = port_aum,
    flat_multiplier = flat_multiplier,
    exclusion = input_data_list$excluded_companies
  )

  cat("-- Calculating credit risk. \n\n\n")

  overall_pd_changes <- annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year_lookup,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  expected_loss <- company_expected_loss(
    data = overall_pd_changes,
    loss_given_default = lgd,
    exposure_at_default = exposure_by_technology_and_company,
    port_aum = port_aum
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient output from overall pd changes or related financial data inputs

  annual_pd_changes <- calculate_pd_change_annual(
    data = annual_profits,
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

iteration_sequence <- function(args_list) {
  iter_var <- get_iter_var(args_list)
  if (identical(iter_var, "standard")) {
    return(1L)
  } else {
    return(seq_along(args_list[[iter_var]]))
  }
}

st_read_specific <- function(dir, asset_type, use_company_terms) {
  out <- list(
    pacta_results = read_pacta_results(pacta_results_file(dir, asset_type)),
    sector_exposures = read_sector_exposures(sector_exposures_file(dir)),
    company_terms = read_company_terms(company_terms_file(dir), use_company_terms)
  )

  return(out)
}

st_read_agnostic <- function(dir, start_year) {
  out <- list(
    capacity_factors = read_capacity_factors(capacity_factor_file(dir)),
    excluded_companies = read_excluded_companies(excluded_companies_file(dir)),
    df_price = read_price_data_old2(price_data_file(dir)),
    scenario_data = read_scenario_data(scenario_data_file(dir, start_year)),
    financial_data = read_financial_data(financial_data_file(dir))
  )

  return(out)
}

st_process <- function(data, asset_type, company_exclusion, term) {
  start_year <- get_start_year(data)
  scenarios_filter <- scenarios_filter()

  sector_exposures <- process_sector_exposures(
    data$sector_exposures,
    asset_type = asset_type
  )

  capacity_factors_power <- process_capacity_factors_power(
    data$capacity_factors,
    scenarios_filter = scenarios_filter,
    scenario_geography_filter = scenario_geography_filter_lookup,
    technologies = technologies_lookup,
    start_year = start_year,
    end_year = end_year_lookup
  )

  excluded_companies <- process_excluded_companies(
    data$excluded_companies,
    company_exclusion = company_exclusion,
    technologies = technologies_lookup
  )

  df_price <- process_df_price(
    data$df_price,
    technologies = technologies_lookup,
    sectors = sectors_lookup,
    start_year = start_year,
    end_year = end_year_lookup
  )

  scenario_data <- process_scenario_data(
    data$scenario_data,
    start_year = start_year,
    end_year = end_year_lookup,
    sectors = sectors_lookup,
    technologies = technologies_lookup,
    scenario_geography_filter = scenario_geography_filter_lookup,
    scenarios_filter = scenarios_filter
  )

  financial_data <- process_financial_data(
    data$financial_data,
    asset_type = asset_type
  )

  company_terms <- process_company_terms(
    data$company_terms,
    fallback_term = term
  )

  pacta_results <- process_pacta_results(
    data$pacta_results,
    start_year = start_year,
    end_year = end_year_lookup,
    time_horizon = time_horizon_lookup,
    scenario_geography_filter = scenario_geography_filter_lookup,
    scenarios_filter = scenarios_filter,
    equity_market_filter = equity_market_filter_lookup,
    sectors = sectors_lookup,
    technologies = technologies_lookup,
    allocation_method = allocation_method_lookup,
    asset_type = asset_type
  ) %>%
    add_terms(company_terms = company_terms, fallback_term = term)

  out <- list(
    pacta_results = pacta_results,
    sector_exposures = sector_exposures,
    capacity_factors_power = capacity_factors_power,
    excluded_companies = excluded_companies,
    df_price = df_price,
    scenario_data = scenario_data,
    financial_data = financial_data,
    company_terms = company_terms
  )

  return(out)
}

pacta_results_file <- function(dir, asset_type) {
  asset_type <- stringr::str_to_title(asset_type)
  file <- glue::glue("{asset_type}_results_{calculation_level_lookup}.rda")
  out <- file.path(dir, file)
  return(out)
}

sector_exposures_file <- function(dir) {
  out <- file.path(dir, "overview_portfolio.rda")
  return(out)
}

capacity_factor_file <- function(dir) {
  out <- file.path(dir, "prewrangled_capacity_factors_WEO_2020.csv")
  return(out)
}

excluded_companies_file <- function(dir) {
  out <- file.path(dir, "exclude-companies.csv")
  return(out)
}

price_data_file <- function(dir) {
  file <- paste0("prices_data_", price_data_version_lookup, ".csv")
  out <- file.path(dir, file)
  return(out)
}

scenario_data_file <- function(dir, start_year) {
  out <- file.path(dir, paste0("Scenarios_AnalysisInput_", start_year, ".csv"))
  return(out)
}

financial_data_file <- function(dir) {
  out <- file.path(dir, "prewrangled_financial_data_stress_test.csv")
  return(out)
}

company_terms_file <- function(dir) {
  out <- file.path(dir, "company_terms.csv")
  return(out)
}

get_start_year <- function(data) {
  out <- min(data$pacta_results$year, na.rm = TRUE)
  return(out)
}

scenarios_filter <- function() {
  out <- unique(c(baseline_scenario_lookup, shock_scenario_lookup))
  return(out)
}
