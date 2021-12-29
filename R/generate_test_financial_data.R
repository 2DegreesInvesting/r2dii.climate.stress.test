#' Generates a toy data set of financial information for example companies
#'
#' @description
#' Takes example company/portfolio data set as input. Generates a data frame
#' with toy financial data information. Contains relevant financial data for
#' profit calculation and Merton model for all companies for which test cases
#' are defined. Currently the financial data is identical for all companies, as
#' the test cases do not test variations in the credit risk part of the model.
#' @param data data frame containing the test financial data set
#' @family example case functions
#' @return data frame
#' @export
generate_test_financial_data <- function(data) {
  test_financial_data <- data %>%
    dplyr::distinct(
      .data$company_name,
      .data$id
    ) %>%
    dplyr::select(
      .data$company_name,
      company_id = .data$id
    ) %>%
    dplyr::mutate(
      corporate_bond_ticker = NA_character_,
      pd = 0.001,
      net_profit_margin = 0.05,
      debt_equity_ratio = 0.1,
      volatility = 0.35
    )
}
