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
