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

#' Check that sector to ald_business_unit mapping corresponds to template
#'
#' @param data A tibble.
#' @param sector_col Name of column holding sector (needed due to naming
#'   inconsistencies.)
#' @param mapper_template A tibble holding, among others, mapping of sectors to
#'   technologies.
#'
#' @return Returns `data` invisibly.
check_sector_tech_mapping <- function(data, sector_col = "ald_sector",
                                      mapper_template = p4i_p4b_sector_technology_lookup) {
  sector_tech_mapper <- mapper_template %>%
    dplyr::select(dplyr::all_of(c("sector_p4i", "technology_p4i"))) %>%
    dplyr::rename(ald_sector = "sector_p4i", ald_business_unit = "technology_p4i") %>%
    dplyr::filter(.data$ald_sector %in% unique(get(sector_col, data)))

  additional_sector_tech_combinations <- data %>%
    dplyr::rename(ald_sector = rlang::sym(sector_col)) %>%
    dplyr::anti_join(sector_tech_mapper, by = c("ald_sector", "ald_business_unit"))

  missing_sector_tech_combinations <- sector_tech_mapper %>%
    dplyr::anti_join(data %>%
      dplyr::rename(ald_sector = rlang::sym(sector_col)), by = c("ald_sector", "ald_business_unit"))

  if (nrow(additional_sector_tech_combinations) > 0 | nrow(missing_sector_tech_combinations) > 0) {
    stop("Incorrect mapping of sectors to technologies indentified in data, please check.")
  }

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
