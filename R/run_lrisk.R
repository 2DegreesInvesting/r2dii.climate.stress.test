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
#' @param lgd Numeric, holding the loss given default, for accepted value range
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
#' @param settlement_factor Catch all factor (ratio) that can be used to adjust the
#'   expected payout of the settlement due to further data gaps. Set to 1 by
#'   default.
#' @param exp_share_damages_paid Numeric. Ratio that defines the expected share
#'   of the calculated social cost of carbon that is considered in the liability.
#'   This can follow historical precedents such as the Tobacco Master
#'   Settlement that had a an expected share of 2.7% of the damages paid.
#' @param scc Numeric. Social cost of carbon per excess ton of CO2 emitted. This
#'   is the price for each surplus ton of CO2 that goes into the calculation of
#'   the carbon liability of a company.
#' @param return_results Boolean, indicating if results shall be exported.
#' @return NULL
#' @export
run_lrisk <- function(asset_type,
                      input_path_project_specific,
                      input_path_project_agnostic,
                      output_path,
                      baseline_scenario = "WEO2020_SPS",
                      shock_scenario = "WEO2020_SDS",
                      lgd = 0.45,
                      risk_free_rate = 0.02,
                      discount_rate = 0.07,
                      growth_rate = 0.03,
                      div_netprofit_prop_coef = 1,
                      shock_year = 2030,
                      fallback_term = 2,
                      scenario_geography = "Global",
                      use_company_terms = FALSE,
                      settlement_factor = 1,
                      exp_share_damages_paid = 0.027,
                      scc = 40L,
                      return_results = FALSE) {
  cat("-- Running litigation risk stress test. \n\n\n")

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()

  iter_var <- get_iter_var(args_list)

  cat("-- Validating input arguments. \n")

  validate_input_values(
    baseline_scenario = baseline_scenario,
    shock_scenario = shock_scenario,
    scenario_geography = scenario_geography,
    lgd = lgd,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    growth_rate = growth_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    fallback_term = fallback_term,
    use_company_terms = use_company_terms,
    asset_type = asset_type,
    settlement_factor = settlement_factor,
    exp_share_damages_paid = exp_share_damages_paid,
    scc = scc,
    risk_type = "lrisk"
  )

  args_list$output_path <- customise_output_path(
    output_path = args_list$output_path,
    iter_var = iter_var
  )

  st_results_list <- run_lrisk_iteration(args_list)

  result_names <- names(st_results_list[[1]])
  st_results <- result_names %>%
    purrr::map(function(tib) {
      purrr::map_dfr(st_results_list, `[[`, tib)
    }) %>%
    purrr::set_names(result_names)

  st_results_aggregated <- aggregate_results(
    results_list = st_results,
    sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup],
    iter_var = iter_var,
    risk_type = "lrisk"
  )

  st_results_wrangled_and_checked <- wrangle_results(
    results_list = st_results_aggregated,
    risk_type = "lrisk",
    sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup]
  ) %>%
    check_results(
      sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup],
      risk_type = "lrisk"
    )

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

run_lrisk_iteration <- function(args_list) {
  run_stress_test_iteration_once <- function(arg_tibble_row) {
    arg_list_row <- arg_tibble_row %>%
      dplyr::select(-.data$return_results) %>%
      as.list()

    arg_tibble_row <- arg_tibble_row %>%
      dplyr::select(-dplyr::all_of(setup_vars_lookup)) %>%
      dplyr::rename_with(~ paste0(.x, "_arg"))

    st_result <- read_and_process_and_calc_lrisk(arg_list_row) %>%
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
globalVariables(c(names(formals(run_lrisk)), "iter_var"))

read_and_process_and_calc_lrisk <- function(args_list) {
  list2env(args_list, envir = rlang::current_env())

  log_path <- file.path(output_path, paste0("log_file_", iter_var, ".txt"))

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

  year_litigation_event <- shock_year

  cat("-- Reading input data from designated input path. \n")

  if (use_company_terms) {
    paste_write("Using user - configured company - term data. \n", log_path = log_path)
  }

  data <- st_read_specific(
    input_path_project_specific,
    asset_type = asset_type,
    use_company_terms = use_company_terms
  )
  start_year <- get_start_year(data)
  data <- append(
    data, st_read_agnostic(input_path_project_agnostic, start_year = start_year, sectors = sectors_and_technologies_list$sectors)
  )

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

  litigation_scenario <- tibble::tibble(
    scenario_name = glue::glue("SCC_{year_litigation_event}"),
    model = "SCC",
    exp_share_damages_paid = exp_share_damages_paid,
    scc = scc,
    start_of_analysis = start_year,
    end_of_analysis = end_year_lookup,
    year_of_shock = year_litigation_event,
    duration_of_shock = end_year_lookup - year_litigation_event + 1
  )

  cat("-- Calculating market risk. \n")

  # TODO: maybe wrap the below into something like calculate_annual_profits_lr()
  # TODO: decide if a slow change in price trajectory is needed...
  # For now, we assume that we just have the standard prices which are renamed to be able to use functions
  price_data <- input_data_list$df_price %>%
    dplyr::rename(
      Baseline_price = !!rlang::sym(glue::glue("price_{baseline_scenario}")),
      late_sudden_price = !!rlang::sym(glue::glue("price_{shock_scenario}"))
    )

  extended_pacta_results <- input_data_list$pacta_results %>%
    extend_scenario_trajectory(
      scenario_data = input_data_list$scenario_data,
      start_analysis = start_year,
      end_analysis = end_year_lookup,
      time_frame = time_horizon_lookup,
      target_scenario = shock_scenario
    ) %>%
    set_baseline_trajectory(
      scenario_to_follow_baseline = baseline_scenario
    ) %>%
    # we currently assume that production levels and emission factors of
    # misaligned company-technology combinations are forced onto the target
    # scenario trajectory directly after the litigation shock.
    # This may not be perfectly realistic and may be refined in the future.
    # TODO: we need to decide how to handle low carbon technologies.
    # currently they are exempt from liabilities of not building out enough.
    # this may be realistic, but misaligned ones should not switch their
    # production to the target trajectory. This would lead to unrealistic jumps
    # in buildout and the litigation risk model should not require solving
    # for the scenario.
    set_litigation_trajectory(
      litigation_scenario = shock_scenario,
      shock_scenario = litigation_scenario,
      litigation_scenario_aligned = shock_scenario,
      start_year = start_year,
      end_year = end_year_lookup,
      analysis_time_frame = time_horizon_lookup,
      log_path = log_path
    ) %>%
    dplyr::mutate(
      actual_emissions = .data$late_sudden * .data$emission_factor,
      allowed_emissions = !!rlang::sym(shock_scenario) * .data$emission_factor,
      overshoot_emissions = dplyr::if_else(
        .data$actual_emissions - .data$allowed_emissions < 0,
        0,
        .data$actual_emissions - .data$allowed_emissions
      )
    )

  if (asset_type == "bonds") {
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker")
  } else {
    merge_cols <- c("company_name")
  }

  extended_pacta_results_with_financials <- extended_pacta_results %>%
    dplyr::inner_join(
      y = input_data_list$financial_data,
      by = merge_cols
    ) %>%
    fill_annual_profit_cols()

  # TODO: check if we want diverging prices between the scenarios at all.
  # these need to be explained, or we need to pick one time series only.
  annual_profits <- extended_pacta_results_with_financials %>%
    join_price_data(df_prices = price_data) %>%
    calculate_net_profits()

  # TODO: validate this section in detail
  annual_profits <- annual_profits %>%
    dplyr::mutate(
      scc_liability =
        .data$overshoot_emissions * litigation_scenario$scc *
          litigation_scenario$exp_share_damages_paid
    ) %>%
    dplyr::group_by(company_name, ald_sector, technology) %>%
    dplyr::mutate(
      settlement = sum(.data$scc_liability, na.rm = TRUE) * settlement_factor
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      net_profits_ls = dplyr::if_else(
        year == litigation_scenario$year_of_shock,
        .data$net_profits_ls - .data$settlement,
        .data$net_profits_ls
      )
    )

  annual_profits <- annual_profits %>%
    dcf_model_techlevel(discount_rate = discount_rate) %>%
    # TODO: ADO 879 - note rows with zero profits/NPVs will produce NaN in the Merton model
    dplyr::filter(!is.na(company_id))

  company_annual_profits <- annual_profits %>%
    calculate_terminal_value(
      end_year = end_year_lookup,
      growth_rate = growth_rate,
      discount_rate = discount_rate,
      baseline_scenario = baseline_scenario,
      shock_scenario = shock_scenario
    )

  company_technology_npv <- company_annual_profits %>%
    company_technology_asset_value_at_risk(
      shock_scenario = litigation_scenario,
      div_netprofit_prop_coef = div_netprofit_prop_coef,
      flat_multiplier = flat_multiplier_lookup,
      crispy = TRUE
    )

  cat("-- Calculating credit risk. \n\n\n")

  company_pd_changes_overall <- company_annual_profits %>%
    calculate_pd_change_overall(
      shock_year = litigation_scenario$year_of_shock,
      end_of_analysis = end_year_lookup,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  company_trajectories <- add_term_to_trajectories(
    annual_profits = company_annual_profits,
    pacta_results = input_data_list$pacta_results
  )

  return(
    list(
      company_pd_changes_overall = company_pd_changes_overall,
      company_trajectories = company_trajectories,
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
