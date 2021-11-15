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

  data <- purrr::map(fs::dir_ls(out), readr::read_csv, col_types = list())
  expect_true(all(purrr::map_lgl(data, ~rlang::has_name(.x, "term_arg"))))
})
