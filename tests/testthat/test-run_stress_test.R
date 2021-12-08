test_that("with bonds output is unchanged and names have the suffix '_arg'", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_BONDS")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("bonds",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      term = 1:2,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))

  csv_files <- fs::dir_ls(out, regexp = "[.]csv$")
  data <- purrr::map(csv_files, readr::read_csv, col_types = list())
  have_suffix_arg <- purrr::map_lgl(data, ~ rlang::has_name(.x, "term_arg"))
  expect_true(all(have_suffix_arg))
})
