#' Check that required technologies are provided
#'
#' @param data A tibble holding at least column `technology`
#' @param expected_technologies String vector holding names of expected
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
      expected_levels_collapsed <- paste0(levels, collapse = ", ")
      provided_levels_collapsed <- paste0(unique(get(var, data)), collapse = ", ")
      rlang::abort(c(
        glue::glue("Data must hold all expected levels on variable {var}."),
        x = glue::glue("Expected levels are {expected_levels_collapsed}, provided levels are {provided_levels_collapsed}."),
        i = "Please check input data."
      ))
    }
  })

  return(invisible(data))
}

check_sector_tech_mapping <- function(data, sector_col, mapper_template = p4i_p4b_sector_technology_lookup) {

  sector_tech_mapper <- mapper_template %>%
    dplyr::select(sector_p4i, technology_p4i) %>%
    dplyr::rename(ald_sector = sector_p4i, technology = technology_p4i)

  missing_sector_tech_combinations <- data %>%
    dplyr::rename(ald_sector = rlang::sym(sector_col)) %>%
    dplyr::anti_join(sector_tech_mapper, by = c("ald_sector", "technology"))

  if (nrow(missing_sector_tech_combinations) > 0) {
    stop("Incorrect mapping of sectors to technologies indentified in data, please check.")
  }

  return(invisible(data))
}
