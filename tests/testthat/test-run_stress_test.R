test_that("output includes argument names with suffix '_arg'", {
  is_me <- identical(path.expand("~"), "/home/mauro")
  skip_if_not(is_me)

  # Refresh
  out_path <- fs::path(Sys.getenv("ST_PROJECT_FOLDER"), "outputs")
  if (fs::dir_exists(out_path)) fs::dir_delete(out_path)

  x <- suppressWarnings(capture.output(
    run_stress_test("bonds", term = 1:2)
  ))

  data <- purrr::map(fs::dir_ls(out_path), readr::read_csv, col_types = list())
  expect_true(all(purrr::map_lgl(data, ~rlang::has_name(.x, "term_arg"))))
})
