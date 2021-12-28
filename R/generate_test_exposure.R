#' Generates a toy PACTA style portfolio with example companies
#'
#' @description
#' Takes example company/portfolio data set as input. Generates a PACTA style
#' exposure data frame. Contains the sector level financial exposures of the toy
#' portfolio to the companies representing stylized cases of interest for the
#' transition risk stress test.
#' @param data data frame containing the test company/portfolio data set
#' @family example case functions
#' @return data frame
#' @export
generate_test_exposure <- function(data) {
  test_portfolio_distribution <- data %>%
    dplyr::filter(.data$year == min(.data$year, na.rm = TRUE)) %>%
    dplyr::distinct(
      .data$investor_name, .data$portfolio_name, .data$financial_sector,
      .data$id, .data$technology, .data$plan_carsten
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$financial_sector
    ) %>%
    dplyr::summarise(
      plan_carsten = sum(.data$plan_carsten, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      asset_type = .env$asset_type_test,
      valid_input = TRUE,
      value = .data$plan_carsten * .env$portfolio_size
    )

  test_exposures <- tibble::tibble(
    investor_name = .env$investor_name_test,
    portfolio_name = .env$portfolio_name_test,
    asset_type = c(
      rep.int(.env$asset_type_test, 10),
      .env$asset_type_test,
      "Others",
      "Unclassifiable"
    ), # potential other types: "Bonds", "Loans", "Funds", "Other", "Unclassifiable"
    financial_sector = c(
      "Automotive",
      "Aviation",
      "Cement",
      "Coal",
      "HDV",
      "Oil&Gas",
      "Other",
      "Power",
      "Shipping",
      "Steel",
      "Other",
      "Power",
      "Unclassifiable"
    ),
    valid_input = c(
      rep.int(TRUE, 10),
      FALSE,
      TRUE,
      FALSE
    ),
    valid_value_usd = NA_real_,
    asset_value_usd = NA_real_,
    portfolio_value_usd = NA_real_,
    currency = "USD"
  )

  # left join the exposures so that the other sectors can be filled with 0 for
  # demo purposes
  test_exposures <- test_exposures %>%
    dplyr::left_join(
      test_portfolio_distribution,
      by = c("investor_name", "portfolio_name", "asset_type", "financial_sector", "valid_input")
    ) %>%
    dplyr::mutate(
      valid_value_usd = dplyr::if_else(!is.na(.data$value), .data$value, 0)
    ) %>%
    dplyr::select(-.data$value) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_type,
      .data$valid_input
    ) %>%
    dplyr::mutate(
      asset_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$valid_input
    ) %>%
    dplyr::mutate(
      portfolio_value_usd = sum(.data$valid_value_usd, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      portfolio_value_usd = dplyr::if_else(
        .data$asset_type == "Unclassifiable",
        .env$portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
        .data$portfolio_value_usd
      ),
      asset_value_usd = dplyr::if_else(
        .data$asset_type == "Unclassifiable",
        .env$portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
        .data$asset_value_usd
      ),
      valid_value_usd = dplyr::if_else(
        .data$asset_type == "Unclassifiable",
        .env$portfolio_size - sum(.data$valid_value_usd, na.rm = TRUE),
        .data$valid_value_usd
      )
    )
}
