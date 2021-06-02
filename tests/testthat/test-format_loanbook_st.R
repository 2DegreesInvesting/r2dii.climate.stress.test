test_that("format_loanbook_st returns a data frame", {
  test_data_lbk_results <- readRDS(testthat::test_path("test_data", "test_data_format_loanbook_st.rds"))

  credit_type <- "loan_share_outstanding"

  formatted_results <- test_data_lbk_results %>%
    format_loanbook_st(credit = credit_type)

  testthat::expect_s3_class(formatted_results, "data.frame")
})

test_that("format_loanbook_st returns error, if wrong credit type specified", {
  test_data_lbk_results <- readRDS(testthat::test_path("test_data", "test_data_format_loanbook_st.rds"))

  wrong_credit_type <- "mumble_jumble"

  testthat::expect_error(
    format_loanbook_st(data = test_data_lbk_results, credit = wrong_credit_type),
    regexp = "credit_allowed is not TRUE",
    fixed = TRUE
  )
})

