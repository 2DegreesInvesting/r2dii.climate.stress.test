#' Read in price data from csv and check that all expected columns are given.
#'
#' @param path A string that points to the location of the file containing the
#'   price data
#' @param version A string indicating whether to prepare the capacity factors
#'   based on the old price data structure or the new long format. Must be
#'   either "old" or "new".
#'
#' @family import functions
#'
#' @export
read_price_data <- function(path = NULL,
                            version = NULL) {
  path %||% stop("Must provide 'path'")
  version %||% stop("Must provide 'version'")

  valid_input_file_path <- file.exists(file.path(path))
  stopifnot(valid_input_file_path)

  version_allowed <- version %in% c("old", "new")
  stopifnot(version_allowed)

  if (version == "new") {
    data <- read_price_data_internal(path = path)
  } else {
    data <- read_price_data_internal_old(path = path)
  }

  return(data)
}
