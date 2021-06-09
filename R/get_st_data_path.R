#' Get the path to the required stress test auxiliary data location. Can be
#' provided either via environment variable or options.
#'
#' @param var String. Provide the name of the environment variable that points
#'   to the data directory/repo. Default is "ST_DATA_PATH". If no environment
#'   variable is provided and the default "ST_DATA_PATH" cannot be found, the
#'   function tries to get the data path from options. In that case, provide
#'   the name of the option that points to the data directory/repo. Default
#'   option to get the path is, again, "ST_DATA_PATH".
#'
#' @family miscellaneous utility functions
#'
#' @return Character
#'
#' @export
get_st_data_path <- function(var = "ST_DATA_PATH") {
  path <- Sys.getenv(var, unset = getOption(var) %||% "unset")

  if (path == "unset") abort("`var` is unset. Please add data path as envvar or R option.")
  path
}
