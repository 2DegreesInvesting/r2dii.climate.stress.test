test_that("with bonds, with iteration, using default settings output is unchanged and names have the suffix '_arg'", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_specific <- Sys.getenv("ST_SPECIFIC_BONDS")
  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("bonds",
      input_path_project_specific = in_specific,
      input_path_project_agnostic = in_agnostic,
      output_path = out,
      shock_year = c(2025, 2030),
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))

  have_suffix_arg <- purrr::map_lgl(results, ~ rlang::has_name(.x, "term_arg"))
  expect_true(all(have_suffix_arg))
})

test_that("with loans, without iteration, using minimum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_specific <- Sys.getenv("ST_SPECIFIC_LOANS")
  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("loans",
      input_path_project_specific = in_specific,
      input_path_project_agnostic = in_agnostic,
      output_path = out,
      lgd_senior_claims = get_st_argument("lgd_senior_claims", "min"),
      lgd_subordinated_claims = get_st_argument("lgd_subordinated_claims", "min"),
      risk_free_rate = get_st_argument("risk_free_rate", "min"),
      discount_rate = get_st_argument("discount_rate", "min"),
      div_netprofit_prop_coef = get_st_argument("div_netprofit_prop_coef", "min"),
      shock_year = get_st_argument("shock_year", "min"),
      term = as.integer(get_st_argument("term", "min")),
      company_exclusion = FALSE,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("with equity, without iteration, using maximum values of input arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_specific <- Sys.getenv("ST_SPECIFIC_EQUITY")
  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("equity",
      input_path_project_specific = in_specific,
      input_path_project_agnostic = in_agnostic,
      output_path = out,
      lgd_senior_claims = get_st_argument("lgd_senior_claims", "max"),
      lgd_subordinated_claims = get_st_argument("lgd_subordinated_claims", "max"),
      risk_free_rate = get_st_argument("risk_free_rate", "max"),
      discount_rate = get_st_argument("discount_rate", "max"),
      div_netprofit_prop_coef = get_st_argument("div_netprofit_prop_coef", "max"),
      shock_year = get_st_argument("shock_year", "max"),
      term = as.integer(get_st_argument("term", "max")),
      company_exclusion = TRUE,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})
