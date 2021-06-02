#' Source scripts quietly based on a list of input paths
#'
#' @param paths A vector of paths to scripts that need to be sourced for running
#'   the work flow at hand. Usually this will point to files that contain
#'   function definitions.

source_all <- function(paths) {
  lapply(paths, source)
  invisible(paths)
}
