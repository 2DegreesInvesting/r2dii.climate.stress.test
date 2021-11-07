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
  testthat::expect_output(object, regexp = NA, ...)
}

expect_no_error <- function(object, ...) {
  testthat::expect_error(object, regexp = NA, ...)
}

# FIXME: Dead code? ----------------------------------------------------------

expect_no_warning <- function(object, ...) {
  testthat::expect_warning(object, regexp = NA, ...)
}

outputs_path <- function() {
  fs::path(Sys.getenv("ST_PROJECT_FOLDER"), "outputs")
}

dir_empty <- function(dir) {
  if (fs::dir_exists(dir)) fs::dir_delete(dir)
  fs::dir_create(dir)
}

is_empty_dir <- function(dir) {
  identical(fs::dir_ls(dir), character(0))
}

ls_results <- function(dir) {
  fs::dir_ls(dir, regexp = "stress_test_results.*[.]csv$")
}
