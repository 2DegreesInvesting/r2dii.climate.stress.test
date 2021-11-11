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

#' Validate that a file exists in a given directory
#'
#' Before performing an operation on a file assumed to be found in a given
#' directory, validate this file exists and give indicative error if not.
#'
#' @param path Character vector indicating the directory of a file.
#'
#' @return NULL
#'
#' @export
validate_file_exists <- function(path) {
  valid_file_path <- file.exists(file.path(path))

  if (!valid_file_path) {
    rlang::abort(c(
      "Detected invalid file path.",
      x = glue::glue("Invalid file path: {file.path(path)}."),
      i = "Did you set path to data correctly?."
    ))
  }
  invisible()
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
    affected_cols <- glue::glue_collapse(sort(setdiff(expected_columns, names(data))), sep = ", ")
    rlang::abort(c(
      "Detected missing columns on data set.",
      x = glue::glue("Missing columns: {affected_cols}."),
      i = "Please check that data have expected columns."
    ))
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

#' Report dropped companies
#'
#' Wrapper to report companies for which all results, or results for some technologies
#' are lsot due to a missing match in financial_data or price_data.
#'
#' @param data_list A list of imported stress test input data.
#' @inheritParams validate_input_values
#'
#' @return NULL
report_company_drops <- function(data_list, asset_type) {

  if (asset_type == "bonds") {
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker")
  } else {
    merge_cols <- c("company_name")
  }

  report_dropped_company_names(
    data_x = data_list$pacta_result,
    data_y = data_list$financial_data,
    name_y = "financial data",
    merge_cols = merge_cols
  )

  report_dropped_company_names(
    data_x = data_list$pacta_result,
    data_y = data_list$df_price,
    name_y = "price data",
    merge_cols = c("technology", "ald_sector" = "sector", "year")
  )

  invisible()
}

#' Inner join datasets and report number of dropped rows
#'
#' Function conducts inner join on two datasets and reports number of dropped
#' rows on `data_x`.
#'
#' @param data_x Tibble with data that is joinable to `data_y`.
#' @param data_y Tibble with data that is joinable to `data_x`.
#' @param name_y Name of `data_x`.
#' @param merge_cols Vector holds columns to join on.
#' @param name_x Name of `data_x, defults to PACTA results.
#'
#' @return The merged dataset.
report_dropped_company_names <- function(data_x, data_y, name_y, merge_cols, name_x = "PACTA results") {

  data <- data_x %>%
    dplyr::inner_join(
      data_y,
      by = merge_cols
    )

  n_companies_x <- length(unique(data_x$company_name))
  n_companies <- length(unique(data$company_name))

  if (n_companies < n_companies_x) {
    percent_loss <- (n_companies_x - n_companies) * 100/n_companies_x
    affected_companies <- sort(setdiff(data_x$company_name, data$company_name))
    cat(
      "      >> When joining", name_x, "on", name_y, "on column(s)", paste0(merge_cols, collapse = ", "), "dropped rows for",
      n_companies_x - n_companies, "out of", n_companies_x, "companies\n"
    )
    cat("        >> percent loss:", percent_loss, "\n")
    cat("        >> affected companies:\n")
    purrr::walk(affected_companies, function(company) {
      cat("          >>", company, "\n")
    })
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
#' @inheritParams validate_input_values
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
#' @inheritParams validate_input_values
#'
#' @return A numeric holding value of lgd.
assign_lgd <- function(asset_type, lgd_senior_claims,
                       lgd_subordinated_claims) {
  lgd <- ifelse(asset_type %in% c("equity", "bonds"), lgd_subordinated_claims, lgd_senior_claims)
  return(lgd)
}

#' Get name of iterator variable
#'
#' Uses fallback if no iterator is used. Aborts if > 1 iterator is given.
#'
#' @param args_list Named list of default and provided arguments in function
#'   call to [run_stress_test()].
#'
#' @return String holding name of iterator variable.
get_iter_var <- function(args_list) {

  iterate_arg <- purrr::map_int(args_list, length) %>%
    tibble::enframe() %>%
    dplyr::filter(.data$value > 1)

  if (nrow(iterate_arg) == 0) {
    iter_var <- "standard"
  } else if (nrow(iterate_arg) == 1) {
    iter_var <- iterate_arg$name

    if (iter_var %in% c("asset_type", path_vars_lookup)) {
      rlang::abort(c(
        "Must not provide more than one value for not iterateable argument",
        x = glue::glue("Arguments with multiple values: {toString(iter_var)}."),
        i = "Please coorect your function call"
        ))
    }
  } else {
    rlang::abort(c(
      "Must provide no more than one argument with multiple values.",
      x = glue::glue("Arguments with multiple values: {toString(iterate_arg$name)}."),
      i = "Did you forget to pick only one?"
    ))
  }

  return(iter_var)
}
