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

#' Get path for stress test masterdata files
#'
#' Return list of paths for stress test masterdata files.
#'
#' @return A list with 3 file paths.
#' @export
create_stressdata_masterdata_file_paths <- function() {

  path_parent <- get_st_data_path()

  paths <- list(
    "prewrangled_financial_data_bonds.rds",
    "prewrangled_financial_data_equity.rds",
    "prewrangled_financial_data_loans.rds"
  ) %>%
    purrr::map(function(file) {
      file_path <- file.path(path_parent, file)
      if (!file.exists(file.path(file_path))) {
        stop("Stresstest master data file does not exist.")
      }
      return(file_path)
    }) %>%
    purrr::set_names(c("bonds", "equity", "loans"))

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
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_columns))

  data_has_expected_columns <-
    all(expected_columns %in% colnames(data))

  if (!data_has_expected_columns) {
    stop(paste0("Detected missing columns: ", paste0(sort(setdiff(
      expected_columns, names(data))
    ), collapse = ", "), "."), call. = FALSE)
  }
  invisible()
}


#' Checks data for missings and duplicates
#'
#' Applies consistency checks to data concerning the combinations of columns
#' that should be unique in combination. In concrete:
#'
#' 1. it is checked if there are duplicate rows.
#' 1. it is checked if there are duplicate rows on `composite_unique_cols`.
#'
#'
#' @param data A tibble.
#' @param composite_unique_cols A vector of names of columns that shall be
#'   unique in their combination.
#' @param throw_error Boolean, if TRUE error is thrown on failures, otherwise a
#'   warning.
#'
#' @return input `data`.
#' @export
report_all_duplicate_kinds <- function(data, composite_unique_cols, throw_error = TRUE) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = composite_unique_cols
  )

  report_duplicates(
    data = data,
    cols = names(data),
    throw_error = throw_error
  )

  report_duplicates(
    data = dplyr::distinct(data),
    cols = composite_unique_cols,
    throw_error = throw_error
  )

  return(invisible(data))
}

#' Identify and report missing value combinations
#'
#' Checks if all level combinations of `composite_unique_cols` are in`data` and
#' throws a warning on missing combinations.
#' NOTE:
#' 1. a combination of all levels is not neccesarily required/useful, make sure
#' to use function only in adequate context.
#' 1. combiantions of too many columns/values may exceed memory size.
#' .
#'
#' @inheritParams report_all_duplicate_kinds
#'
#' @return NULL
#' @export
report_missing_col_combinations <- function(data, composite_unique_cols, throw_error = FALSE) {
  all_combinations <- data %>%
    tidyr::expand(!!!rlang::syms(composite_unique_cols))

  missing_rows <- all_combinations %>%
    dplyr::anti_join(data, by = composite_unique_cols)

  if (nrow(missing_rows) > 0) {
    if (throw_error) {
      stop(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(composite_unique_cols, collapse = ", "), "."))
    } else {
      warning(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(composite_unique_cols, collapse = ", "), "."), call. = FALSE)
    }
  }

  return(invisible())
}

#' Report duplicate rows
#'
#' Reports duplicates in `data` on columns `cols`. Duplicates are reported via a
#' warning.
#'
#' @inheritParams report_all_duplicate_kinds
#' @param cols Cols to check for duplicate combinations on.
#'
#' @return NULL
report_duplicates <- function(data, cols, throw_error = TRUE) {
  duplicates <- data %>%
    dplyr::group_by(!!!rlang::syms(cols)) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::select(!!!rlang::syms(cols)) %>%
    dplyr::distinct_all()

  if (nrow(duplicates) > 0) {
    if (throw_error) {
      stop(paste0("Identified ", nrow(duplicates), " duplicates on columns ", paste(cols, collapse = ", "), "."))
    } else {
      warning(paste0("Identified ", nrow(duplicates), " duplicates on columns ", paste(cols, collapse = ", "), "."), call. = FALSE)
    }
  }

  return(invisible())
}


#' Inner join datasets and report number of dropped rows
#'
#' Function conducts inner join on two datasets and reports number of dropped
#' rows on `data_x`.
#'
#' @param data_x Tibble with data that is joinable to `data_y`.
#' @param data_y Tibble with data that is joinable to `data_x`.
#' @param name_x Name of `data_x`.
#' @param name_y Name of `data_x`.
#' @param merge_cols Vector holds columns to join on.
#'
#' @return The merged dataset.
inner_join_report_drops <- function(data_x, data_y, name_x, name_y, merge_cols) {

  rows_x <- nrow(data_x)
  rows_y <- nrow(data_y)

  data <- data_x %>%
    dplyr::inner_join(
      data_y,
      by = merge_cols
    )

  rows_data <- nrow(data)

  if (rows_data < rows_x) {
    cat(
      "      >> When joining", name_x, "on", name_y, "on columns", merge_cols, "dropped",
      rows_x - rows_data, "rows from", name_x, "\n"
    )
  }
  return(data)
}

#' Report missing
#'
#' Function reports number of missing values per variable.
#'
#' @inheritParams report_all_duplicate_kinds
#' @param data Tibble holding a result data set.
#' @param name_data Name of the data file.
#'
#' @return input `data`.
report_missings <- function(data, name_data, throw_error = FALSE) {
  missings <- purrr::map_df(data, function(x) sum(is.na(x)))

  if (is_verbose_log_env()) {
    cat("Reporting missings on dataset:", name_data, "\n")
    purrr::iwalk(missings, function(n_na, name) {
      cat("Counted", n_na, "missings on column", name, "\n")
    })
    cat("\n\n")
  }

  if (throw_error && rowSums(missings) > 0) {
    stop(paste0("Missings detected on ", name_data, ", please check dataset."), call. = FALSE)
  }

  invisible(data)
}


#' Assign value of flat multiplier
#'
#' Assign value of flat multiplier based on `asset_type`.
#'
#' @inheritParams run_stress_test
#'
#' @return A double holding value of the flat multiplier.
assign_flat_multiplier <- function(asset_type) {
  flat_multiplier <- ifelse(asset_type %in% c("loans", "bonds"),  0.15, 1.0)
  return(flat_multiplier)
}

#' Assign value of lgd
#'
#' Assigns value of lgd based on `asset_type`. Can be from `lgd_senior_claims`
#' or `lgd_subordinated_claims`.
#'
#' @inheritParams run_stress_test
#'
#' @return A numerix holding value of lgd.
assign_lgd <- function(asset_type, lgd_senior_claims,
                       lgd_subordinated_claims) {
  lgd <- ifelse(asset_type %in% c("equity", "bonds"), lgd_subordinated_claims, lgd_senior_claims)
  return(lgd)
}
