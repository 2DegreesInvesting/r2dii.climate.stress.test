test_that("output includes argument names with suffix '_arg'", {
  is_me <- identical(path.expand("~"), "/home/mauro")
  skip_if_not(is_me)

  in_agnostic <- "/home/mauro/tmp/st/ST_INPUTS_MASTER"
  in_specific <- "/home/mauro/tmp/st/ST_TESTING_BONDS/inputs"
  out <- tempfile()
  fs::dir_create(out)

  x <- suppressWarnings(capture.output(
    run_stress_test("bonds",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      term = 1:2
    )
  ))

  csv_files <- fs::dir_ls(out, regexp = "[.]csv$")
  data <- purrr::map(csv_files, readr::read_csv, col_types = list())
  cols_from_arguments_have_suffix_arg <- purrr::map_lgl(data, ~rlang::has_name(.x, "term_arg"))
  expect_true(all(cols_from_arguments_have_suffix_arg))
})

test_that("with multiple inputs to non-iterable arguments errors gracefully", {
  too_long <- 1:2
  expect_snapshot_error(
    run_stress_test(asset_type = too_long, "a", "b", tempdir())
  )

  # FIXME: Not the error I expect
  expect_snapshot_error(
    run_stress_test(asset_type = too_long)
  )
})
