test_that("degrees returns '\u00B0'", {
  expect_equal(degrees(), "\u00B0")
})

test_that("path_dropbox_2dii output is as expected", {
  expect_equal(
    fs::path_file(path_dropbox_2dii()),
    as.character(glue::glue("Dropbox (2{degrees()} Investing)"))
  )
})

test_that("path_dropbox_2dii output is an fs path", {
  expect_s3_class(path_dropbox_2dii(), "fs_path")
})

test_that("path_dropbox_2dii outputs on object of correct class", {
  expect_s3_class(path_dropbox_2dii(), "fs_path")
})

test_that("path_dropbox_2dii outputs a path to an existing directory", {
  skip_if_not(dropbox_exists())
  expect_true(fs::dir_exists(path_dropbox_2dii()))
})

test_that("r2dii_dropbox_path works with a custom Dropbox folder", {
  out <- withr::with_options(
    list(r2dii_dropbox = "custom dropbox"),
    path_dropbox_2dii("a", "path")
  )

  pattern <- fs::path("custom dropbox", "a", "path")
  expect_true(grepl(pattern, out))
})

# report_missing_col_combinations -----------------------------------------
test_that("No warning is thrown if combinations are exhaustive", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2", "A2"),
    b = c("B1", "B2", "B1", "B2"),
    c = 1:4
  )

  expect_silent(report_missing_col_combinations(
    data = data,
    composite_unique_cols = c("a", "b")
  ))
})

test_that("Warning is thrown if combinations are missing", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2"),
    b = c("B1", "B2", "B1"),
    c = 1:3
  )

  expect_warning(
    report_missing_col_combinations(
      data = data,
      composite_unique_cols = c("a", "b")
    ),
    "Identified 1 missing combinations"
  )
})

# report_duplicates -------------------------------------------------------
test_that("No warning is thrown if no duplicates are in data on composite unique cols.", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2", "A2"),
    b = c("B1", "B2", "B1", "B2"),
    c = 1:4
  )

  expect_silent(report_duplicates(
    data = data,
    cols = c("a", "b")
  ))
})

test_that("Warning is thrown if there are duplicates on composite unique cols", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2", "A2", "A2"),
    b = c("B1", "B2", "B1", "B2", "B1"),
    c = 1:5
  )

  expect_warning(
    checked_data <- report_duplicates(
      data = data,
      cols = c("a", "b")
    ),
    "Identified 1 duplicates"
  )
})

test_that("Error is thrown if there are duplicates on composite unique cols and throw_error is TRUE", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2", "A2", "A2"),
    b = c("B1", "B2", "B1", "B2", "B1"),
    c = 1:5
  )

  expect_error(
    checked_data <- report_duplicates(
      data = data,
      cols = c("a", "b"),
      throw_error = TRUE
    ),
    "Identified 1 duplicates"
  )
})
test_that("Warning is thrown if there are duplciates on all cols", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A2", "A2", "A2"),
    b = c("B1", "B2", "B1", "B2", "B1")
  )

  expect_warning(
    report_duplicates(
      data = data,
      cols = names(data)
    ),
    "Identified 1 duplicates"
  )
})

# report_all_duplicate_kinds ----------------------------------------------
test_that("Warnings are thrown for dataset with full and partial duplicates", {
  data <- tibble::tibble(
    a = c("A1", "A1", "A1", "A2"),
    b = c("B1", "B1", "B1", "B1"),
    c = c(1, 1, 2, 3)
  )

  expect_error(expect_warning(report_all_duplicate_kinds(
    data = data,
    composite_unique_cols = c("a", "b")
  )))
})
