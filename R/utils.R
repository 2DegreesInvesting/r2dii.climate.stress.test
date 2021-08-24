#' Does this computer have a local copy of 2dii's dropbox folder?
#'
#' @family miscellaneous utility functions
#'
#' @return Logical.
#'
#' @export
#' @examples
#' dropbox_exists()
dropbox_exists <- function() {
  fs::dir_exists(path_dropbox_2dii())
}

#' Insert the symbol for degrees
#'
#' @family miscellaneous utility functions
#'
#' @return A string.
#'
#' @export
#' @examples
#' degrees()
#' glue::glue("2{degrees()} Investing Initiative")
degrees <- function() {
  "\u00B0"
}

#' Easily access directories in your local copy of 2dii's Dropbox folder
#'
#' These functions create cross-platform paths pointing to 2dii's Dropbox
#' folder:
#' Your projects may need data stored in 2dii's Dropbox folder. Sometimes it is
#' convenient to have your projects close to the data. But, in this case, it is
#' a bad idea because the path to 2dii's Dropbox folder has a problematic space
#' and symbol. (For example, [RStudio's Git pane may not
#' work](https://github.com/2DegreesInvesting/resources/issues/51).) Instead,
#' place your projects somewhere with a sane path, such as
#' `C:/Users/You/git/project/`, and access the data you need with
#' `path_dropbox_2dii()`.
#'
#' @section Setup for a custom Dropbox folder:
#' If the name of your 2dii Dropbox folder is different from the default,
#' you may add this to .Rprofile (see [usethis::edit_r_profile()]):
#'
#' ```
#' options(r2dii_dropbox = "The name of your custom dropbox folder goes here")
#' ````
#'
#' @param ... Character vectors, if any values are `NA`, the result will also be
#'   `NA`.
#'
#' @seealso [degrees()], [fs::path_home()].
#' @family functions to output 2dii paths
#'
#' @return A character string.
#'
#' @export
path_dropbox_2dii <- function(...) {
  custom <- getOption("r2dii_dropbox")
  default <- sprintf("Dropbox (2%s Investing)", degrees())
  fs::path_home(custom %||% default, ...)
}

#' Write error logs to the project log files. Appends the most recent message
#' to that file.
#'
#' @param msg A string containing the error message
#' @param location A string containing the path to the project directory
#' @param ... unnamed arguments are pasted at the end of the log message.
#'
#' @family miscellaneous utility functions
#'
#' @return A string.
#'
#' @export
write_log <- function(msg,
                      location,
                      ...) {
  composed <- paste(
    as.character(Sys.time()),
    as.character(msg),
    ...
  )
  write(
    composed,
    file = glue::glue("{location}/00_Log_Files/error_messages.txt"),
    append = TRUE
  )
}

#' Get path for stress test masterdata files
#'
#' Get path for stress test masterdata files when called in 2dii internal mode.
#'
#' @param data_prep_timestamp Character scalar holding timestamp of data prep.
#' @param twodii_internal Boolean, if TRUE 2dii internal mode is used.
#'
#' @return A list with 2 file paths.
#' @export
create_stressdata_masterdata_file_paths <- function(data_prep_timestamp, twodii_internal) {
  if (!twodii_internal) {
    stop("Currently cannot provide data files for external mode.")
  }

  if (!is.character(data_prep_timestamp)) {
    stop("Timestamp is not provided in correct format")
  }

  path_parent <- path_dropbox_2dii("PortCheck", "00_Data", "07_AnalysisInputs", data_prep_timestamp)

  paths <- list(
    "stress_test_masterdata_debt.rda",
    "stress_test_masterdata_ownership.rda"
  ) %>%
    purrr::map(function(file) {
      file_path <- file.path(path_parent, file)
      if (!file.exists(file.path(file_path))) {
        stop("Stresstest master data file does not exist.")
      }
      return(file_path)
    }) %>%
    purrr::set_names(c("bonds", "listed_equity"))

  return(paths)
}

