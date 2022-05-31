#' Does this computer have a local copy of 2dii's dropbox folder?
#'
#' @family miscellaneous utility functions
#'
#' @return Logical.
#'
#' @examples
#' r2dii.climate.stress.test:::dropbox_exists()
dropbox_exists <- function() {
  fs::dir_exists(path_dropbox_2dii())
}

#' Insert the symbol for degrees
#'
#' @family miscellaneous utility functions
#'
#' @return A string.
#'
#' @examples
#' r2dii.climate.stress.test:::degrees()
#' glue::glue("2{r2dii.climate.stress.test:::degrees()} Investing Initiative")
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
#' @return String holding provided `path`.
#' @export
validate_file_exists <- function(path) {
  valid_file_path <- file.exists(path)

  if (!valid_file_path) {
    rlang::abort(c(
      "Path must point to an existing file.",
      x = glue::glue("Invalid file path: {file.path(path)}."),
      i = "Did you set path to data correctly?."
    ))
  }
  invisible(path)
}

#' Validate that a data frame contains expected columns
#'
#' Validate that all expected columns for an operation are given in a data frame.
#'
#' @param data data frame that is to be validated
#' @param expected_columns Character vector listing the expected columns
#'
#' @return NULL
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
      "Must include expected columns in data set.",
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
#' 1. a combination of all levels is not necessarily required/useful, make sure
#' to use function only in adequate context.
#' 1. combinations of too many columns/values may exceed memory size.
#' .
#'
#' @inheritParams report_all_duplicate_kinds
#' @param col_names String holding names of columns.
#'
#' @return Returns `data` invisibly.
report_missing_col_combinations <- function(data, col_names, throw_error = FALSE) {
  all_combinations <- data %>%
    tidyr::expand(!!!rlang::syms(col_names))

  missing_rows <- all_combinations %>%
    dplyr::anti_join(data, by = col_names)

  if (nrow(missing_rows) > 0) {
    if (throw_error) {
      stop(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(col_names, collapse = ", "), "."))
    } else {
      warning(paste0("Identified ", nrow(missing_rows), " missing combinations on columns ", paste(col_names, collapse = ", "), "."), call. = FALSE)
    }
  }

  return(invisible(data))
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
#' @param log_path String holding path to log file.
#' @inheritParams validate_input_values
#'
#' @return NULL
report_company_drops <- function(data_list, asset_type, log_path) {
  if (asset_type == "bonds") {
    merge_cols <- c("company_name", "id" = "corporate_bond_ticker")
  } else {
    merge_cols <- c("company_name")
  }

  report_dropped_company_names(
    data_x = data_list$pacta_results,
    data_y = data_list$financial_data,
    name_y = "financial data",
    merge_cols = merge_cols,
    log_path = log_path
  )

  report_dropped_company_names(
    data_x = data_list$pacta_results,
    data_y = data_list$df_price,
    name_y = "price data",
    merge_cols = c("technology", "ald_sector", "year"),
    log_path = log_path
  )

  invisible()
}

#' Inner join datasets and report number of dropped rows
#'
#' Function conducts inner join on two datasets and reports number of dropped
#' rows on `data_x`.
#'
#' @param data_x Tibble with data that can be joined to `data_y`.
#' @param data_y Tibble with data that can be joined to `data_x`.
#' @param name_y Name of `data_x`.
#' @param merge_cols Vector holds columns to join on.
#' @param name_x Name of `data_x, defaults to PACTA results.
#' @inheritParams report_company_drops
#'
#' @return The merged dataset.
report_dropped_company_names <- function(data_x, data_y, name_y, merge_cols, name_x = "PACTA results", log_path) {
  data <- data_x %>%
    dplyr::inner_join(
      data_y,
      by = merge_cols
    )

  n_companies_x <- length(unique(data_x$company_name))
  n_companies <- length(unique(data$company_name))

  if (n_companies < n_companies_x) {
    percent_loss <- (n_companies_x - n_companies) * 100 / n_companies_x
    affected_companies <- sort(setdiff(data_x$company_name, data$company_name))
    paste_write(
      format_indent_1(), "When joining", name_x, "on", name_y, "on column(s)", paste0(merge_cols, collapse = ", "), "could not match entries for",
      n_companies_x - n_companies, "out of", n_companies_x, "companies.",
      log_path = log_path
    )
    paste_write(format_indent_2(), "percent loss:", percent_loss, log_path = log_path)
    paste_write(format_indent_2(), "affected companies:", log_path = log_path)
    purrr::walk(affected_companies, function(company) {
      paste_write(format_indent_2(), company, log_path = log_path)
    })
    paste_write("\n", log_path = log_path)
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
  missings_per_col <- purrr::map_df(data, function(x) sum(is.na(x)))

  has_missings <- rowSums(missings_per_col)

  if (has_missings) {
    cat("Reporting missings on dataset:", name_data, "\n")
    purrr::iwalk(as.list(missings_per_col), function(n_na, name) {
      if (n_na > 0) {
        cat("Counted", n_na, "missings on column", name, "\n")
      }
    })
    cat("\n\n")
  }

  if (throw_error && has_missings) {
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
  flat_multiplier <- ifelse(asset_type %in% c("loans", "bonds"), 0.15, 1.0)
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

    if (iter_var %in% c("asset_type", "fallback_term", setup_vars_lookup)) {
      rlang::abort(c(
        "Must not provide more than one value for argument that cannot be iterated",
        x = glue::glue("Arguments with multiple values: {toString(iter_var)}."),
        i = "Please correct your function call"
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

#' Helper function for logging
#'
#' Wrapper around [write()] that concatenates objects passed in `...` and
#' appends per default.
#' @noRd
paste_write <- function(..., log_path, append = TRUE) {
  text <- paste(...)
  write(text, file = log_path, append = append)
  invisible()
}

# helper functions to indent lines in logfile
format_indent_1 <- function() {
  ">>"
}
format_indent_2 <- function() {
  "  >>"
}


#' Checks if input args are missing
#'
#' Checks if any list entries in `args_list` are symbols. Called with
#' `args_list` as input argument this serves as a proxy for checking is
#' arguments are missing.
#'
#' @param args_list A named list.
#'
#' @return returns `args_list` invisibly.
fail_if_input_args_are_missing <- function(args_list) {
  missings <- purrr::map(args_list, is.symbol) %>%
    purrr::keep(isTRUE)

  if (length(missings) > 0) {
    missing_args <- glue::glue_collapse(names(missings), sep = ", ")
    rlang::abort(
      c(
        "All input arguments need to hold values.",
        x = glue::glue("Missing arguments: {missing_args}."),
        i = "Did you provide all input arguemnts correctly?."
      )
    )
  }

  invisible(args_list)
}

#' Customise output path
#'
#' Checks for existence of provided `output_path` and extends it to hold a
#' subdirectory named after the timestamp and `iter_var`.
#'
#' @param output_path String holding path to st output folder.
#' @param iter_var String holding name of iteration variable.
#'
#' @return Path to subdirectory in st output folder.
customise_output_path <- function(output_path, iter_var) {
  if (!dir.exists(output_path)) {
    rlang::abort(
      c(
        "Argument output_path must point to an existing directory.",
        x = glue::glue("Invalid file path: {output_path}."),
        i = "Did you set output_path correctly?."
      )
    )
  }

  timestamp <- paste(Sys.time(), iter_var, sep = "_")
  timestamp <- gsub(" ", "_", timestamp)
  timestamp <- gsub(":", "-", timestamp)
  output_path_custom <- file.path(output_path, timestamp)

  dir.create(output_path_custom)

  # FIXME: quick solution to avoid empty output dirs in case of failing calculations
  paste_write("Starting analysis.", log_path = file.path(output_path_custom, paste0("log_file_", iter_var, ".txt")))

  return(output_path_custom)
}

stop_if_empty <- function(data, data_name) {
  if (nrow(data) == 0) {
    rlang::abort(glue::glue("Stopping calculation, dataset {data_name} is empty."))
  }
  return(invisible(data))
}

get_start_year <- function(data) {
  out <- min(data$pacta_results$year, na.rm = TRUE)
  return(out)
}

infer_sectors_and_technologies <- function(baseline_scenario, shock_scenario, scenario_geography) {
browser()
  sectors_baseline <- scenario_geography_x_ald_sector %>%
    dplyr::filter(.data$scenario == !!baseline_scenario & .data$scenario_geography == !!scenario_geography) %>%
    dplyr::pull(.data$ald_sector)

  sectors_shock <- scenario_geography_x_ald_sector %>%
    dplyr::filter(.data$scenario == !!shock_scenario & .data$scenario_geography == !!scenario_geography) %>%
    dplyr::pull(.data$ald_sector)

  shared_sectors <- dplyr::intersect(sectors_baseline, sectors_shock)

  if (length(shared_sectors) == 0) {
    rlang::abort(
      c(
        "Could not find sectors that are supported for baseline and shock scenario for selected scenario_geography.",
        x = glue::glue("baseline scenario: {baseline_scenario}, shock_scenario: {shock_scenario}, scenario_geography: {scenario_geography}"),
        i = "Please use function scenario_for_sector_x_geography to identify a valid combination."
      )
    )
  }

  technologies <- p4i_p4b_sector_technology_lookup %>%
    dplyr::filter(.data$sector_p4i %in% !!shared_sectors) %>%
    dplyr::pull(.data$technology_p4i)

  return(list(sectors = shared_sectors, technologies = technologies))
}
