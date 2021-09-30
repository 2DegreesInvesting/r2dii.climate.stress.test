#' Wrangles and checks sector exposures
#'
#' Wrangles sector exposures by removing invalid rows and rows from assets other
#' than `asset_types`. Also checks for duplicates and that more than 0 rows
#' remain.
#'
#' @param sector_exposures A tibble holding sector_exposures as calculated by
#'   PACTA.
#' @param asset_type Type of analyzed asset. Can be bonds or equity.
#'
#' @return `sector_exposures` holding only valid rows for `asset_type`
#' @export
wrangle_and_check_sector_exposures_eq_cb <- function(sector_exposures, asset_type) {

  if (!asset_type %in% c("Bonds", "Equity")) {
    stop("Can only wrangle dataset for asset types bonds and equity.")
  }

  valid_sector_exposures <- sector_exposures %>%
    dplyr::filter(valid_input) %>%
    dplyr::filter(asset_type == !!asset_type) %>%
    dplyr::select(-valid_input, asset_type)

  if (nrow(valid_sector_exposures) == 0) {
    ("Stop, no valid sector exposures available.")
  }

  report_all_duplicate_kinds(valid_sector_exposures, cuc_sector_exposures_eq_cb)

  return(valid_sector_exposures)
}
