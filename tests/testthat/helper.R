#' Helper function that gets csv test data quietly
#'
#' @family helper functions for testthat
#'
#' @return A dataframe
read_test_data <- function(file, path = testthat::test_path("test_data", file)) {
  readr::read_csv(path, col_types = list())
}

#' Helper function that sets up temporary target directory, if it does not exist
#'
#' @param path Character. Main path to the directory the output file should be
#'   written to.
#' @param add_level Character. String with an investor or portfolio name that is
#'   added to the initial path as an extra layer to the output directory.
#'   This is where the file will end up being written to.
#' @family helper functions for testthat
#'
#' @return A dataframe
test_create_target_directory <- function(path = NULL,
                                         add_level = NULL) {
  path <- path %||% stop("Must provide 'path'")
  add_level <- add_level %||% stop("Must provide 'add_level'")

  if (!dir.exists(file.path(path, add_level))) {
    target_dir <- dir.create(file.path(path, add_level))
  } else {
    target_dir <- file.path(path, add_level)
  }
}


#' Get a valid value of an argument of `run_stress_test()`
#'
#' @examples
#' get_st_argument("fallback_term")
#' # Same
#' get_st_argument("fallback_term", "default")
#'
#' get_st_argument("fallback_term", "min")
#'
#' typeof(get_st_argument("asset_type"))
#' @noRd
get_st_argument <- function(name,
                            value = c("default", "allowed", "min", "max")) {
  stopifnot(is.character(name), length(name) == 1)

  value <- match.arg(value)
  out <- stress_test_arguments

  out <- out[out$name == name, c("type", value)]
  as_type <- get(paste0("as.", out$type))
  out <- as_type(out[[value]])
  out
}
