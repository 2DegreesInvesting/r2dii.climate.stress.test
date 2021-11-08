#' Sensitivity analysis: `run_stress_test()` multiple times
#'
#' These functions performs the so called "sensitivity analysis" for the asset
#' type in their name. They run [run_stress_test()] multiple times, iterating
#' over multiple values of one and only one argument of [run_stress_test()].
#'
#' @param data Character vector of length 2. Paths to the directories
#'   ST_INPUTS_MASTER and ST_TESTING_\{aset-type\}, e.g. ST_TESTING_BONDS,
#'   respectively. These directories must have the files expected by the
#'   stress-test project. The easiest way to create this object is with
#'   [st_data_paths()].
#' @param destdir Character. Path to a destination directory.
#' @param ... Arguments passed to [run_stress_test()].
#' @param quiet Logical. Print non-condition messages to the console?
#'
#' @seealso [st_data_paths()].
#' @family main functions
#'
#' @return A data frame.
#'
#' @export
#'
#' @examplesIf r2dii.climate.stress.test:::is_registered_dev("/home/mauro")
#' library(readr, warn.conflicts = FALSE)
#'
#' # Give the paths to your data explicitely
#' data <- st_data_paths(
#'   data = "/home/mauro/tmp/st/ST_INPUTS_MASTER",
#'   project = "/home/mauro/tmp/st/ST_TESTING_BONDS"
#' )
#' out <- suppressWarnings(
#'   st_bonds(data, term = c(1, 2))
#' )
#'
#' # Or set these environment variables (e.g. in .Renviron), then omit `data`
#' data
#'
#' # Here we omit `data` and make the console output more verbose
#' out <- st_bonds(term = c(1, 2), quiet = FALSE)
#'
#' # The data frame output helps you quickly explore and manipulate your results
#' subset(out, st_name == "port" & arg_value == 2)
#'
#' # If you run out of memory, instead write output sequentially to disk
#' path <- file.path(tempdir(), "st")
#' dir.create(path)
#' suppressWarnings(
#'   st_write_bonds(destdir = path, term = c(1, 2))
#' )
#' list.files(path)
#'
#' # Later you may read all results at once
#' files <- list.files(path, full.names = TRUE)
#' results <- read_csv(files, id = "arg_val", show_col_types = FALSE)
#' results
#'
#' # The column `arg_val` helps you identify each run
#' unique(
#'   subset(results, grepl("term.*2", arg_val), select = 1:3)
#' )
st_bonds <- function(data = st_data_paths(), ..., quiet = TRUE) {
  st_df(data, asset_type = "bonds", ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_equity <- function(data = st_data_paths(), ..., quiet = TRUE) {
  st_df(data, asset_type = "equity", ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_loans <- function(data = st_data_paths(), ..., quiet = TRUE) {
  st_df(data, asset_type = "loans", ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_write_bonds <- function(data = st_data_paths(),
                           destdir = tempdir(),
                           ...,
                           quiet = TRUE) {
  st_write(data, asset_type = "bonds", destdir = destdir, ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_write_equity <- function(data = st_data_paths(),
                            destdir = tempdir(),
                            ...,
                            quiet = TRUE) {
  st_write(data, asset_type = "equity", destdir = destdir, ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_write_loans <- function(data = st_data_paths(),
                           destdir = tempdir(),
                           ...,
                           quiet = TRUE) {
  st_write(data, asset_type = "loans", destdir = destdir, ..., quiet = quiet)
}

st_df <- function(data, asset_type, ..., quiet = TRUE) {
  args <- enlist_args(data, asset_type, ..., quiet = quiet)

  args %>%
    map(~ exec(st, !!!.x)) %>%
    enframe(value = "st_result") %>%
    restructure_st_df() %>%
    unnest(st_result)
}

st_write <- function(data, asset_type, destdir = tempdir(), ..., quiet = TRUE) {
  args <- enlist_args(data, asset_type, ..., quiet = quiet)

  for (i in seq_along(args)) {
    out <- exec(st, !!!args[[i]])
    out <- unnest(out, .data$st_result)
    write_csv(out, file = path(destdir, glue("{names(args)[[i]]}.csv")))
  }

  invisible(data)
}

enlist_args <- function(data, asset_type, ..., quiet) {
  dots <- list2(...)
  long <- keep(dots, ~ length(.x) > 1L) %>%
    abort_if_no_argument_is_long() %>%
    abort_if_more_than_one_argument_is_long()

  dots1 <- dots[setdiff(names(dots), names(long))]
  args1 <- list2(data = data, asset_type = asset_type, !!!dots1, quiet = quiet)
  nms <- names(long)
  val <- unlist(long)

  vec_set_names(val, glue("{nms}___{val}")) %>%
    map(~ append(args1, vec_set_names(.x, nms)))
}

abort_if_no_argument_is_long <- function(data) {
  if (identical(length(data), 0L)) {
    abort(c(
      "Must privide one argument of `run_stress_test()` with multiple values.",
      i = "Do you need to use `run_stress_test()` instead?"
    ))
  }

  invisible(data)
}

abort_if_more_than_one_argument_is_long <- function(data) {
  if (length(data) > 1L) {
    abort(c(
      "Must provide no more than one argument with multiple values.",
      x = glue("Arguments with multiple values: {toString(names(data))}."),
      i = "Did you forget to pick only one?"
    ))
  }

  invisible(data)
}

restructure_st_df <- function(data) {
  data %>%
    separate(.data$name, into = c("arg_name", "arg_value"), sep = "___") %>%
    unnest(cols = .data$st_result) %>%
    relocate(.data$st_type, .data$st_name)
}

st <- function(data, asset_type, ..., quiet = TRUE) {
  vec_assert(data, character(), size = 2L)
  data <- vec_set_names(data, envvar_keys())
  local_envvar(data)

  control_verbosity <- ifelse(quiet, utils::capture.output, identity)
  invisible(control_verbosity(run_stress_test(asset_type = asset_type, ...)))

  outputs_path() %>%
    dir_ls() %>%
    map(~ read_csv(.x, show_col_types = FALSE)) %>%
    enframe(value = "st_result") %>%
    restructure_st()
}

restructure_st <- function(data) {
  data %>%
    mutate(name = path_file(name)) %>%
    mutate(name = path_ext_remove(name)) %>%
    mutate(name = sub("^stress_test_results_", "", name)) %>%
    extract(name, into = c("st_type", "st_name"), "([^_]+)_(.*)")
}
