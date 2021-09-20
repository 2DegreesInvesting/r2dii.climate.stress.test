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
    "prewrangled_financial_data_bonds.rds",
    "prewrangled_financial_data_equity.rds",
    "stress_test_masterdata_credit.rds"
  ) %>%
    purrr::map(function(file) {
      file_path <- file.path(path_parent, file)
      if (!file.exists(file.path(file_path))) {
        stop("Stresstest master data file does not exist.")
      }
      return(file_path)
    }) %>%
    purrr::set_names(c("bonds", "listed_equity", "loans"))

  return(paths)
}

#' Validate that a file exists in a given directory
#'
#' Before performing an operation on a file assumed to be found in a given
#' directory, validate this file exists and give indicative error if not.
#'
#' @param path Character vector indicating the directory of a file
#'
#' @return A boolean.
#'
#' @export
validate_file_exists <- function(path) {
  valid_file_path <- file.exists(file.path(path))
  stopifnot(valid_file_path)
}

#' Validate that a data frame contains expected columns
#'
#' Validate that all expected columns for an operation are given in a data frame.
#'
#' @param data data frame that is to be validated
#' @param expected_columns Character vector listing the expected columns
#'
#' @return A boolean.
#'
#' @export
validate_data_has_expected_cols <- function(data,
                                            expected_columns) {
  data_has_expected_columns <- all(expected_columns %in% colnames(data))
  stopifnot(data_has_expected_columns)
}

#' Checks data for missings and duplicates
#'
#' Applies consistency checks to data concerning the combinations of columns
#' that should exist and be unique in combination. In concrete:
#'
#' 1. it is checked if there are missing combinations of `composite_unique_cols`.
#' 1. it is checked if there are duplicate rows.
#' 1. it is checked if there are duplicate rows on `composite_unique_cols`.
#'
#' Missings/duplicates are reported via a warning. Function is currently not
#' used in code but helps for data wrangling/data research tasks. It will be
#' added to critical data sets in the future.
#'
#' @param data A tibble.
#' @param composite_unique_cols A vector of names of columns that shall be
#'   unique in their combination.
#'
#' @return NULL
#' @export
check_row_consistency <- function(data, composite_unique_cols) {

  validate_data_has_expected_cols(
    data = data,
    expected_columns = composite_unique_cols
  )

  report_missing_col_combinations(
    data = data,
    composite_unique_cols = composite_unique_cols
  )

  report_duplicates(data = data, cols = names(data))

  report_duplicates(data = dplyr::distinct(data), cols = composite_unique_cols) # removed duplicates on all columns

  return(invisible())
}

#' Identify and report missing value combinations
#'
#' Identifies and reports missing value combinations in `data` on
#' `composite_unique_cols`.
#'
#' @inheritParams check_row_consistency
#'
#' @return NULL
report_missing_col_combinations <- function(data, composite_unique_cols) {

  all_combinations <- data %>%
    tidyr::expand(!!!dplyr::syms(composite_unique_cols))

  missing_rows <- all_combinations %>%
    dplyr::anti_join(data, by = composite_unique_cols)

  if (nrow(missing_rows) > 0) {
    warning(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(composite_unique_cols, collapse = ", "), "."))
  }

  return(invisible())
}

#' Report duplicate rows
#'
#' Reports duplicates in `data` on columns `cols`.
#'
#' @inheritParams check_row_consistency
#' @param cols Cols to check for duplicate combinations on.
#'
#' @return NULL
report_duplicates <- function(data, cols) {
  duplicates <- data %>%
    dplyr::group_by(!!!dplyr::syms(cols)) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::select(!!!dplyr::syms(cols)) %>%
    dplyr::distinct_all()

  if (nrow(duplicates) > 0) {
    warning(paste0("Identified ", nrow(duplicates), " duplicates on columns ", paste(cols, collapse = ", "), "."))
  }

  return(invisible())
}
