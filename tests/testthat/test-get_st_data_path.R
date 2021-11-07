test_that("works with `var` set as environmnetal variable", {
  path <- "/as/envar"
  withr::local_envvar(list(ST_DATA_PATH = path))

  expect_equal(get_st_data_path(), path)
})

test_that("works with `var` set as an R option", {
  path <- "/as/options"
  withr::local_envvar(list(ST_DATA_PATH = NULL))
  withr::local_options(list(ST_DATA_PATH = path))

  expect_equal(get_st_data_path(), path)
})

test_that("if `var` is unset errors gracefully", {
  withr::local_envvar(list(ST_DATA_PATH = NULL))
  testexpect_error(get_st_data_path(), "var.*unset")
})
