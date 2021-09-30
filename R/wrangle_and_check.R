wrangle_and_check_sector_exposures_eq_cb <- function(sector_exposures) {
browser()
  valid_sector_exposures <- sector_exposures %>%
    dplyr::filter(valid_input) %>%
    dplyr::select(-valid_input)

  report_all_duplicate_kinds(valid_sector_exposures, cuc_sector_exposures_eq_cb)

  return(valid_sector_exposures)
}
