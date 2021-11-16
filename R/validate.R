#' Check that input values are valid
#'
#' Checks that user inputs are of length 1 and within defined ranges.
#'
#' @inheritParams run_stress_test
#'
#' @return NULL
validate_input_values <- function(lgd_senior_claims, lgd_subordinated_claims,
                                  terminal_value, risk_free_rate, discount_rate,
                                  div_netprofit_prop_coef, shock_year, term,
                                  company_exclusion, asset_type) {
  input_args <- mget(names(formals()), sys.frame(sys.nframe()))

  if (any(purrr::map_int(input_args, length) != 1)) {
    stop("Input arguments to stress test run need to be of length 1")
  }

  c("company_exclusion", "asset_type") %>%
    purrr::walk(validate_value_in_values, args_list = input_args)

  c("lgd_senior_claims", "lgd_subordinated_claims", "terminal_value", "risk_free_rate",
    "discount_rate", "div_netprofit_prop_coef", "shock_year", "term") %>%
    purrr::walk(validate_value_in_range, args_list = input_args)

  if (!shock_year %% 1 == 0) {
    stop("Argmuent shock_year must be a whole number")
  }

  # ADO 1943 - Once we decide to add a separate Merton calculation on the average
  # maturity of a portfolio, this check will need to be removed
  if (!term %% 1 == 0) {
    stop("Argmemnt term must be a whole number")
  }
}

#'  Validate that value in within values
#'
#' #' Checks that value of variable `var` is in valid values as defined in
#' r2dii.climate_stress_test::stress_test_arguments.
#'
#' @param var String holding name of variable.
#' @param args_list Named list holding arguments of parent function call and
#'   their values.
#'
#' @return NULL
validate_value_in_values <- function(var, args_list) {
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
    stopifnot(is.logical(arg_val))
  }

  if (!arg_val %in% allowed_values) {
    allowed_values_collapsed <- paste0(allowed_values, collapse = ", ")
    rlang::abort(
      c(
        glue::glue("Must provide valid input for argument {var}."),
        x = glue::glue("Invalid input: {arg_val}."),
        i = glue::glue("Valid values are: {allowed_values_collapsed}.")
      )
    )
  }
  invisible()
}

#' Validate that value in within range
#'
#' Checks that value of variable `var` is in valid range as defined in
#' r2dii.climate_stress_test::stress_test_arguments.
#'
#' @inheritParams validate_value_in_values
#'
#' @return NULL
validate_value_in_range <- function(var, args_list) {
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
  stopifnot(is.numeric(arg_val))

  if (!dplyr::between(arg_val, min, max)) {
    rlang::abort(
      c(
        glue::glue("Must provide valid input for argument {var}."),
        x = glue::glue("Invalid input: {arg_val}."),
        i = glue::glue("Is your argument in valid range between {min} and {max}?")
      )
    )
  }
  invisible()
}
