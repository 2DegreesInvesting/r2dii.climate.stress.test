#' Check that required levels of a variable are available in data
#'
#' @param data A tibble.
#' @param data_name Character, holding name of dataset.
#' @param expected_levels_list List holding variable names along with their
#'   expected levels. Variables must be present in data.
#' @param throw_error Boolean indicating if to throw error. If FALSE warning is
#'   signaled instead.
#'
#' @return Returns `data` invisibly.
check_level_availability <- function(data, data_name, expected_levels_list, throw_error = TRUE) {
  purrr::iwalk(expected_levels_list, function(levels, var) {
    if (!all(levels %in% unique(get(var, data)))) {
      expected_levels_collapsed <- paste0(levels, collapse = ", ")
      provided_levels_collapsed <- paste0(unique(get(var, data)), collapse = ", ")
      if (throw_error) {
        rlang::abort(c(
          glue::glue("Dataset {data_name} must hold all expected levels on variable {var}."),
          x = glue::glue("Expected levels are {expected_levels_collapsed}, provided levels are {provided_levels_collapsed}."),
          i = "Please check input data."
        ))
      } else {
        rlang::warn(c(
          glue::glue("Dataset {data_name} does not hold all expected levels on variable {var}."),
          x = glue::glue("Expected levels are {expected_levels_collapsed}, provided levels are {provided_levels_collapsed}."),
          i = "Please check input data if this does not match you expectations."
        ))
      }
    }
  })

  return(invisible(data))
}

#' Check expected missing patterns
#'
#' On certain variables missings appear that are not problematic but are
#' expected. Here it is checked whether those missings adhere to expected
#' patterns.
#'
#' @param data Data that expected missings are checked on.
#'
#' @return Returns input invisibly.
check_expected_missings <- function(data) {
  n_missings_production_plan_company_technology <- sum(is.na(data$production_plan_company_technology))
  expected_n_missings_production_plan_company_technology <- as.integer(round(nrow(data) * (length(unique(data$year)) - (time_horizon_lookup + 1)) / length(unique(data$year))))

  if (n_missings_production_plan_company_technology != expected_n_missings_production_plan_company_technology) {
    cat("-- Company Trajectories: Detected unexpected missings on variable production_plan_company_technology. \n")
  }

  return(invisible(data))
}
