#' Path to stress-test inputs
#'
#' This function helps expose the paths to `run_stress_test()` inputs.
#'
#' @param data Character. Path to the data directory.
#' @param project Character. Path to the project directory.
#'
#' @return A character vector.
#'
#' @family helpers
#'
#' @export
#'
#' @examples
#' library(withr)
#'
#' st_data_paths()
#'
#' st_data_paths("/my/data", "/my/project/")
#'
#' local({
#'   local_envvar(st_data_paths("/my/data", "/my/project/"))
#'
#'   print(Sys.getenv("ST_DATA_PATH"))
#'   print(Sys.getenv("ST_PROJECT_FOLDER"))
#' })
#' Sys.getenv("ST_DATA_PATH")
#' Sys.getenv("ST_PROJECT_FOLDER")
st_data_paths <- function(data = Sys.getenv("ST_DATA_PATH"),
                          project = Sys.getenv("ST_PROJECT_FOLDER")) {
  vec_assert(data, character(), 1L)
  vec_assert(project, character(), 1L)
  vec_set_names(c(data, project), envvar_keys())
}

#' The key component of the key-value pair of stress-test environment-variables
#' @examples
#' envvar_keys()
#' @noRd
envvar_keys <- function() {
  c("ST_DATA_PATH", "ST_PROJECT_FOLDER")
}
