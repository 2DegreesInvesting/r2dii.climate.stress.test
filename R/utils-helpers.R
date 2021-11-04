#' Temporarily set the option `r2dii.climate.stress.verbose_log_env` to `TRUE`
#'
#' This is useful to control verbosity of log output, since detailed diagnostic
#' logs may extend the information level that is relevant for a user:
#' * `verbose_log_env()`: Sets the option to `TRUE`, locally.
#' * `is_verbose_log_env()`: Returns the value of the option (`TRUE` or `FALSE`).
#'
#' @examples
#' local({
#'   verbose_log_env()
#'   is_verbose_log_env()
#' })
#' is_verbose_log_env()
#' @noRd
verbose_log_env <- function(envir = parent.frame()) {
  old <- options(r2dii.climate.stress.verbose_log_env = TRUE)
  withr::defer(options(old), envir = envir)
  invisible(old)
}

is_verbose_log_env <- function() {
  getOption("r2dii.climate.stress.verbose_log_env") %||% FALSE
}
