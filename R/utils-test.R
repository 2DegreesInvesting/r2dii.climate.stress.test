#' Temporarily set the option `r2dii.climate.stress.dev_env` to `TRUE`
#'
#' This is useful to test functions that use this option e.g. to return early
#' and skip a long-running process:
#' * `local_dev_env()`: Sets the option to `TRUE`, locally.
#' * `is_dev_env()`: Returns the value of the option (`TRUE` or `FALSE`).
#'
#' @examples
#' local({
#'   local_dev_env()
#'   is_dev_env()
#' })
#' is_dev_env()
#' @noRd
local_dev_env <- function(envir = parent.frame()) {
  old <- options(r2dii.climate.stress.dev_env = TRUE)
  withr::defer(options(old), envir = envir)
  invisible(old)
}

is_dev_env <- function() {
  getOption("r2dii.climate.stress.dev_env") %||% FALSE
}
