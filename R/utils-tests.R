expect_no_error <- function(object, ...) {
  testthat::expect_error(object, regexp = NA, ...)
}
