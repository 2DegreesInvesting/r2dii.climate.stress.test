#' Check that required technologies are provided
#'
#' @param data A tibble holding at least column `technology`
#' @param expectected_technologies String vector holding names of expected
#'   technologies.
#'
#' @return Returns `data` invisibly.
check_technology_availability <- function(data, expected_technologies) {
  if (!all(expected_technologies %in% unique(data$technology))) {
    missing_technologies <- paste0(setdiff(expected_technologies, unique(data$technology)), collapse = ", ")
    rlang::abort(c(
      "Data must hold all expected technologies.",
      x = glue::glue("Missing technologies: {missing_technologies}."),
      i = "Please check input data."
    ))
  }

  return(invisible(data))
}

#' Check that required levels of a variable are available in data
#'
#' @param data A tibble.
#' @param expected_levels_list List holding variable names along with their
#'   expected levels. Variables must be present in data.
#'
#' @return Returns `data` invisibly.
check_level_availability <- function(data, expected_levels_list) {
  purrr::iwalk(expected_levels_list, function(levels, var) {
    if (!all(levels %in% unique(get(var, data)))) {

      rlang::abort(c(
        glue::glue("Data must hold all expected levels on variable {var}."),
        x = glue::glue("Expected levels are {paste0(levels, collapse = ", ")}, provided levels are {paste0(unique(get(var, data)), collapse = ", ")."),
        i = "Please check input data."
      ))
    }
  })

  return(invisible(data))
}
