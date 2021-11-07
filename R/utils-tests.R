#' Allow running tests selectively, only for registered developers
#'
#' @return Logical.
#'
#' @examples
#' is_registered_dev()
#' is_registered_dev("home/you/")
#' @noRd
is_registered_dev <- function(my_home = "/home/mauro") {
  identical(path.expand("~"), my_home)
}

expect_no_output <- function(object, ...) {
  expect_output(object, regexp = NA, ...)
}

expect_no_error <- function(object, ...) {
  expect_error(object, regexp = NA, ...)
}

outputs_path <- function() {
  path(Sys.getenv("ST_PROJECT_FOLDER"), "outputs")
}
