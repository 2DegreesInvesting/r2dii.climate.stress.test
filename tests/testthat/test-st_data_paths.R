test_that("returns the expected output", {
  skip_if_not(is_registered_dev())

  expect_named(st_data_paths(), st_envvar_names())
  expect_true(all(grepl("^ST_", path_file(st_data_paths()))))
})
