#' Check that input values are valid
#'
#' Checks that user inputs are within defined ranges.
#'
#' @inheritParams run_lrisk
#' @param risk_type String that is either lrisk or trisk.
#' @return NULL
validate_input_values <- function(baseline_scenario, shock_scenario, scenario_geography,
                                  lgd, risk_free_rate, discount_rate, growth_rate,
                                  div_netprofit_prop_coef, shock_year,
                                  fallback_term, use_company_terms, asset_type,
                                  settlement_factor, exp_share_damages_paid, scc, risk_type) {

  input_args <- mget(names(formals()), sys.frame(sys.nframe()))
  input_args <- input_args[ - which(names(input_args) == "risk_type")]

  if (!(risk_type %in% c("trisk","lrisk"))) {
    stop("risk type must be either lrisk or trisk")
  }

  if(risk_type == "trisk") {
    input_args[which(names(input_args)%in%c("settlement_factor","scc", "exp_share_damages_paid"))]<-NULL
  }

  c("baseline_scenario", "shock_scenario", "scenario_geography", "asset_type", "use_company_terms") %>%
    purrr::walk(validate_values_in_values, args_list = input_args)

  vector_numeric_args <-  c(
    "lgd", "risk_free_rate", "discount_rate", "growth_rate",
    "div_netprofit_prop_coef", "shock_year", "fallback_term",
    "settlement_factor", "exp_share_damages_paid", "scc")

  if (risk_type == "trisk") {
  vector_numeric_args <- vector_numeric_args[! vector_numeric_args %in% c("settlement_factor", "exp_share_damages_paid", "scc")]
  }
  vector_numeric_args %>%
    purrr::walk(validate_values_in_range, args_list = input_args)

  if (!all(shock_year %% 1 == 0)) {
    stop("Argument shock_year must be a whole number")
  }

  # ADO 1943 - Once we decide to add a separate Merton calculation on the average
  # maturity of a portfolio, this check will need to be removed
  if (!all(fallback_term %% 1 == 0)) {
    stop("Argument fallback_term must be a whole number")
  }

  if (!(growth_rate < discount_rate)) {
    stop("Growth rate needs to be strictly smaller than discount rate")
  }
}

#'  Validate that values are in within values
#'
#' #' Checks that values of variable `var` are in valid values as defined in
#' r2dii.climate_stress_test::stress_test_arguments.
#'
#' @param var String holding name of variable.
#' @param args_list Named list holding arguments of parent function call and
#'   their values.
#'
#' @return NULL
validate_values_in_values <- function(var, args_list) {
  arg_type <- stress_test_arguments %>%
    dplyr::filter(.data$name == .env$var) %>%
    dplyr::pull(.data$type)

  allowed_values <- stress_test_arguments %>%
    dplyr::filter(.data$name == .env$var) %>%
    dplyr::pull(.data$allowed) %>%
    strsplit(",") %>%
    purrr::pluck(1) %>%
    purrr::map_chr(trimws) # FIXME: configure in stress_test_arguments without whitespace

  arg_val <- get(var, args_list)

  if (arg_type == "logical") {
    allowed_values <- as.logical(allowed_values)
    if (!is.logical(arg_val)) {
      rlang::abort(
        c(
          glue::glue("Must provide valid data type for variable {var}."),
          x = glue::glue("Invalid type: {typeof(arg_val)}."),
          i = glue::glue("Valid type is: logical.")
        )
      )
    }
  }

  if (!all(arg_val %in% allowed_values)) {
    arg_vals_invalid <- arg_val[!(arg_val %in% allowed_values)]
    arg_vals_invalid_collapsed <- paste0(arg_vals_invalid, collapse = ", ")
    allowed_values_collapsed <- paste0(allowed_values, collapse = ", ")
    rlang::abort(
      c(
        glue::glue("Must provide valid input for argument {var}."),
        x = glue::glue("Invalid input: {arg_vals_invalid_collapsed}."),
        i = glue::glue("Valid values are: {allowed_values_collapsed}.")
      )
    )
  }
  invisible()
}

#' Validate that values are within range
#'
#' Checks that values of variable `var` are in valid range as defined in
#' r2dii.climate_stress_test::stress_test_arguments.
#'
#' @inheritParams validate_values_in_values
#'
#' @return NULL
validate_values_in_range <- function(var, args_list) {
  min <- stress_test_arguments %>%
    dplyr::filter(.data$name == .env$var) %>%
    dplyr::pull(.data$min) %>%
    as.numeric()

  stopifnot(length(min) == 1)

  max <- stress_test_arguments %>%
    dplyr::filter(.data$name == .env$var) %>%
    dplyr::pull(.data$max) %>%
    as.numeric()

  stopifnot(length(max) == 1)

  arg_val <- get(var, args_list)

  if (!is.numeric(arg_val)) {
    rlang::abort(
      c(
        glue::glue("Must provide valid data type for variable {var}."),
        x = glue::glue("Invalid type: {typeof(arg_val)}."),
        i = glue::glue("Valid type is: numeric.")
      )
    )
  }

  if (!all(dplyr::between(arg_val, min, max))) {
    arg_vals_invalid <- arg_val[!(dplyr::between(arg_val, min, max))]
    arg_vals_invalid_collapsed <- paste0(arg_vals_invalid, collapse = ", ")
    rlang::abort(
      c(
        glue::glue("Must provide valid input for argument {var}."),
        x = glue::glue("Invalid input: {arg_vals_invalid_collapsed}."),
        i = glue::glue("Is your argument in valid range between {min} and {max}?")
      )
    )
  }
  invisible()
}
