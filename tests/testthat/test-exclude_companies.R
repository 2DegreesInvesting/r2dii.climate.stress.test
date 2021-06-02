test_that("without specified arguments, exclude_companies throws error", {
  testthat::expect_error(
    exclude_companies(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for exclusion, exclude_companies throws error", {
  test_comp_annual_profits <- read_test_data("company_annual_profits.csv")

  testthat::expect_error(
    exclude_companies(data = test_comp_annual_profits),
    "Must provide input for 'exclusion'"
  )
})

test_that("baseline and late_sudden values only for excluded company are changed", {
  test_comp_annual_profits <- read_test_data("company_annual_profits.csv")
  test_exclude_companies <- read_test_data("exclude_companies.csv")

  test_scenario_baseline <- "NPS"
  test_scenario_ls <- "SDS"

  test_excluded <- exclude_companies(
    data = test_comp_annual_profits,
    exclusion = test_exclude_companies,
    scenario_baseline = test_scenario_baseline,
    scenario_ls = test_scenario_ls
  )

  input_not_excluded <- test_comp_annual_profits %>%
    dplyr::filter(
      !(company_name %in% test_exclude_companies$company_name &
        technology %in% test_exclude_companies$technology)
    )
  input_excluded <- test_comp_annual_profits %>%
    dplyr::filter(
      company_name %in% test_exclude_companies$company_name &
        technology %in% test_exclude_companies$technology
    )

  output_not_excluded <- test_excluded %>%
    dplyr::filter(
      !(company_name %in% test_exclude_companies$company_name &
        technology %in% test_exclude_companies$technology)
    )
  output_excluded <- test_excluded %>%
    dplyr::filter(
      company_name %in% test_exclude_companies$company_name &
        technology %in% test_exclude_companies$technology
    )

  testthat::expect_equivalent(
    input_not_excluded,
    output_not_excluded
  )
  testthat::expect_equal(unique(output_not_excluded$baseline), 100)
  testthat::expect_equal(unique(input_excluded$baseline), 100)
  testthat::expect_equal(unique(output_excluded$baseline), 0)
  testthat::expect_equal(unique(output_not_excluded$late_sudden), 100)
  testthat::expect_equal(unique(input_excluded$late_sudden), 100)
  testthat::expect_equal(unique(output_excluded$late_sudden), 0)
})
