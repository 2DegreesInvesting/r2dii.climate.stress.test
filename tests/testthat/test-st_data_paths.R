test_that("returns the expected output", {
  expect_named(st_data_paths(), st_envvar_names())
  expect_true(all(grepl("^ST_", path_file(st_data_paths()))))
})
