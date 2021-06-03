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
#' @export
#' @examples
#' data_path()
data_path <- function(..., data_store = "data-raw") {
  fs::path(data_store, ...)
}

#' Get the path to the required stress test auxiliary data location. Can be
#' provided either via environment variable or options.
#'
#' @param envvar String. Provide the name of the environment variable that points
#'   to the data directory/repo. Default is "ST_DATA_PATH".
#' @param option String. If no environment variable is provided and the default
#'   "ST_DATA_PATH" cannot be found, the function tries to get the data path
#'   from options. In that case, provide the name of the option that points
#'   to the data directory/repo. Default option to get the path is "st_data_path".
#'
#' @family miscellaneous utility functions
#'
#' @return Character
#'
#' @export
get_st_data_path <- function(
  envvar = "ST_DATA_PATH",
  option = "st_data_path"
) {
  if (Sys.getenv(envvar) != "") {
    return(Sys.getenv(envvar))
  } else if (!is.null(options(option)[[option]])) {
    return(options(option)[[option]])
  } else {
    stop("Please add data path as envvar or R option")
  }
}
