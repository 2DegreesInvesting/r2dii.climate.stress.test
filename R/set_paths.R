#' conveniently access stress test related files in the directory set via envvar
#' or options
#'
#' @param ... Character vectors, if any values are `NA`, the result will also be
#'   `NA`.
#' @param data_store String. Directory that contains the relevant data for the stress test analysis
#'
#' @family miscellaneous utility functions
#'
#' @return Character
#'
#' @examples
#' r2dii.climate.stress.test:::data_path()
data_path <- function(..., data_store = "data-raw") {
  fs::path(data_store, ...)
}
