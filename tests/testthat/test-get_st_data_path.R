test_that("default returns an fs path", {
  withr::with_envvar(
    new = c("ST_DATA_PATH" = "path/to/data"),
    testthat::expect_s3_class(get_st_data_path(), "fs_path")
  )
})

test_that("non existing envvar and undefined options return error", {
  testthat::expect_error(
    get_st_data_path(envvar = "MISSING"),
    "Please add data path as envvar or R option"
  )
})
