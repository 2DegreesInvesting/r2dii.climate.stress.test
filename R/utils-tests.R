expect_no_error <- function(object, ...) {
  testthat::expect_error(object, regexp = NA, ...)
}

#' Help skip tests where the developer has not opted in running snapshots
#'
#' To opt in set the environment variable `ST_OPT_IN_SNAPSHOTS = TRUE`, maybe
#' via `usethis::edit_r_environ("project")`.
#'
#' @examples
#' default <- list(ST_OPT_IN_SNAPSHOTS = FALSE)
#' withr::with_envvar(default, testthat::skip_if_not(opt_in_snapshots()))
#'
#' opt_in <- list(ST_OPT_IN_SNAPSHOTS = TRUE)
#' withr::with_envvar(opt_in, testthat::skip_if_not(opt_in_snapshots()))
#' @noRd
opt_in_snapshots <- function() {
  out <- Sys.getenv("ST_OPT_IN_SNAPSHOTS", unset = "FALSE")
  as.logical(out)
}
