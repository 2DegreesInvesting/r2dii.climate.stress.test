test_that("with bonds, with iteration, using default settings output is unchanged
          and names have the suffix '_arg'", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

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

  have_suffix_arg <- purrr::map_lgl(results, ~ rlang::has_name(.x, "term_arg"))
  expect_true(all(have_suffix_arg))
})

test_that("with loans, without iteration, using minimum values of input
          arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_LOANS")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("loans",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      lgd_senior_claims = as.numeric(dplyr::filter(stress_test_arguments, name == "lgd_senior_claims")$min),
      lgd_subordinated_claims = as.numeric(dplyr::filter(stress_test_arguments, name == "lgd_subordinated_claims")$min),
      risk_free_rate = as.numeric(dplyr::filter(stress_test_arguments, name == "risk_free_rate")$min),
      discount_rate = as.numeric(dplyr::filter(stress_test_arguments, name == "discount_rate")$min),
      div_netprofit_prop_coef = as.numeric(dplyr::filter(stress_test_arguments, name == "div_netprofit_prop_coef")$min),
      shock_year = as.numeric(dplyr::filter(stress_test_arguments, name == "shock_year")$min),
      term = as.integer(dplyr::filter(stress_test_arguments, name == "term")$min),
      company_exclusion = FALSE,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})

test_that("with equity, without iteration, using maximum values of input
          arguments output is unchanged", {
  skip_if_not(opt_in_snapshots())
  skip_on_ci()
  skip_on_cran()
  skip_slow_tests()

  in_agnostic <- Sys.getenv("ST_AGNOSTIC")
  in_specific <- Sys.getenv("ST_SPECIFIC_EQUITY")
  out <- tempfile()
  fs::dir_create(out)

  suppressed_console_output <- suppressWarnings(capture.output(
    results <- run_stress_test("equity",
      input_path_project_agnostic = in_agnostic,
      input_path_project_specific = in_specific,
      output_path = out,
      lgd_senior_claims = as.numeric(dplyr::filter(stress_test_arguments, name == "lgd_senior_claims")$max),
      lgd_subordinated_claims = as.numeric(dplyr::filter(stress_test_arguments, name == "lgd_subordinated_claims")$max),
      risk_free_rate = as.numeric(dplyr::filter(stress_test_arguments, name == "risk_free_rate")$max),
      discount_rate = as.numeric(dplyr::filter(stress_test_arguments, name == "discount_rate")$max),
      div_netprofit_prop_coef = as.numeric(dplyr::filter(stress_test_arguments, name == "div_netprofit_prop_coef")$max),
      shock_year = as.numeric(dplyr::filter(stress_test_arguments, name == "shock_year")$max),
      term = as.integer(dplyr::filter(stress_test_arguments, name == "term")$min),
      company_exclusion = TRUE,
      return_results = TRUE
    )
  ))

  expect_snapshot(lapply(results, as.data.frame))
})
