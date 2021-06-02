test_that("default will return character path 'data-raw'", {
  expect_equal(data_path(), fs::path("data-raw"))
})

test_that("default returns an fs path", {
  expect_s3_class(data_path(), "fs_path")
})

test_that("adding an unnamed character input will extend the default path", {
  expect_equal(data_path("more"), fs::path("data-raw", "more"))
})

test_that("adding an unnamed character input returns an fs path", {
  expect_s3_class(data_path("more"), "fs_path")
})

test_that("adding multiple unnamed character inputs will extend the default path", {
  expect_equal(data_path("more-data", "even-more-data"), fs::path("data-raw", "more-data", "even-more-data"))
})

test_that("adding NA as input returns fs_path NA", {
  expect_equal(data_path(NA), fs::path(NA))
})

test_that("adding an unnamed character and NA as inputs will return NA", {
  expect_equal(data_path("more-data", NA), fs::path(NA))
})

test_that("adding an unnamed numeric input will coerce the number and extend the default path", {
  expect_s3_class(data_path(1), "fs_path")
})

test_that("adding an unnamed logical input will coerce the logical and extend the default path", {
  expect_equal(data_path(FALSE), fs::path("data-raw", "FALSE"))
})
