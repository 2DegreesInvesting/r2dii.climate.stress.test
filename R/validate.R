#' Check that input values are valid
#'
#' Checks that user inputs are within defined ranges.
#'
#' @inheritParams run_stress_test
#'
#' @return NULL
validate_input_values <- function(lgd_senior_claims, lgd_subordinated_claims,
                                  risk_free_rate, discount_rate,
                                  div_netprofit_prop_coef, shock_year, term,
                                  company_exclusion, asset_type) {
  input_args <- mget(names(formals()), sys.frame(sys.nframe()))

  c("company_exclusion", "asset_type") %>%
    purrr::walk(validate_values_in_values, args_list = input_args)

  c("lgd_senior_claims", "lgd_subordinated_claims", "risk_free_rate",
    "discount_rate", "div_netprofit_prop_coef", "shock_year", "term") %>%
    purrr::walk(validate_values_in_range, args_list = input_args)

  if (!all(shock_year %% 1 == 0)) {
    stop("Argmuent shock_year must be a whole number")
  }

  # ADO 1943 - Once we decide to add a separate Merton calculation on the average
  # maturity of a portfolio, this check will need to be removed
  if (!all(term %% 1 == 0)) {
    stop("Argument term must be a whole number")
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
