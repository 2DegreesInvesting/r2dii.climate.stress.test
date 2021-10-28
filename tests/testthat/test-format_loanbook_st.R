test_that("format_loanbook_st returns a data frame", {
  test_data_lbk_results <- readRDS(testthat::test_path("test_data", "test_data_format_loanbook_st.rds"))

  test_investor_name <- "Meta Investor"
  test_portfolio_name <- "Meta Portfolio"
  test_equity_market <- "GlobalMarket"
  test_credit <- "outstanding"

  formatted_results <- test_data_lbk_results %>%
    format_loanbook_st(
      investor_name = test_investor_name,
      portfolio_name = test_portfolio_name,
      equity_market = test_equity_market,
      credit = test_credit
    )

  testthat::expect_s3_class(formatted_results, "data.frame")
})
