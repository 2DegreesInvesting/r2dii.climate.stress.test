
#' Remove negative late and sudden rows
#'
#' Function checks for negative values on variable late_and_sudden. All
#' technology x company_name combinations holding >= 1 negative value are
#' removed.
#'
#' @param data_with_late_and_sudden A tibble containing scenario data with
#'   projected late and sudden trajectory.
#'
#' @return Input tibble with potentially removed rows.
filter_negative_late_and_sudden <- function(data_with_late_and_sudden) {
  negative_late_and_sudden <- data_with_late_and_sudden %>%
    dplyr::filter(.data$late_and_sudden < 0) %>%
    dplyr::select(.data$company_name, .data$technology) %>%
    dplyr::distinct()

  if (nrow(negative_late_and_sudden) > 0) {
    n_rows_before_removal <- nrow(data_with_late_and_sudden)

    data_with_late_and_sudden <-
      data_with_late_and_sudden %>%
      dplyr::anti_join(negative_late_and_sudden, by = c("company_name", "technology"))

    warning(paste0("Removed ", n_rows_before_removal - nrow(data_with_late_and_sudden),
                   " rows due to negative late and sudden targets."))

    if (nrow(data_with_late_and_sudden) == 0) {
      stop("No rows remain after removing negative late and sudden trajectories.")
    }
  }

  return(data_with_late_and_sudden)
}
