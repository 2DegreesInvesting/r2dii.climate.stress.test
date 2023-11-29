# convert_cap_to_generation -----------------------------------------------
test_that("without specified arguments, function throws error", {
  testthat::expect_error(
    convert_cap_to_generation(),
    "argument \"data\" is missing"
  )
})

test_that("with missing argument for capacity_factors_power,
          function throws error", {
  test_data <- read_test_data("convert_cap_to_generation.csv")

  testthat::expect_error(
    convert_cap_to_generation(data = test_data),
    "Must provide input for 'capacity_factors_power'"
  )
})

test_that("multiplies the power capacity by hours per year and the capacity
          factor so that we get MWh for plan_tech_prod and scen_tech_prod", {
  test_data <- read_test_data("convert_cap_to_generation.csv")
  test_capacity_factors <- read_test_data("capacity_factors.csv")

  test_results <- convert_cap_to_generation(
    data = test_data,
    capacity_factors_power = test_capacity_factors
  )

  verify_input_data <- test_data %>%
    dplyr::filter(ald_business_unit == "NuclearCap")
  verify_capacity_factors <- test_capacity_factors %>%
    dplyr::filter(.data$ald_business_unit == "NuclearCap") %>%
    dplyr::pull(.data$capacity_factor)
  hours_to_year <- 24 * 365
  verify_multiplier <- hours_to_year * verify_capacity_factors

  verify_results <- test_results %>%
    dplyr::filter(ald_business_unit == "NuclearCap")

  testthat::expect_equal(
    verify_results$scen_tech_prod, verify_input_data$scen_tech_prod * verify_multiplier
  )

  testthat::expect_equal(
    verify_results$plan_tech_prod, verify_input_data$plan_tech_prod * verify_multiplier
  )
})

# convert_power_cap_to_generation -----------------------------------------
new_capacity_factors <- tibble::tibble(
  ald_business_unit = c(rep("HydroCap", 8), rep("NuclearCap", 8)),
  scenario_geography = c(rep(c(rep("Global", 4), rep("Brazil", 4)), 2)),
  scenario = c(rep(c(rep("NPS", 2), rep("SDS", 2)), 4)),
  year = c(rep(c(2021, 2022), 8)),
  capacity_factor = c(rep(c(rep(0.5, 2), rep(0.3, 2)), 4))
)

# reducing new_test_data to data with matching years in new_capacity_factors
new_test_data <- read_test_data("convert_power_cap_to_generation.csv") %>%
  dplyr::filter(.data$year %in% c(2021, 2022))

test_that("error is thrown if baseline scenario is missing from input data", {
  testthat::expect_error(
    convert_power_cap_to_generation(
      data = new_test_data,
      capacity_factors_power = new_capacity_factors,
      baseline_scenario = "NOT_A_SCENARIO",
      target_scenario = "SDS"
    ),
    "Missing columns: NOT_A_SCENARIO"
  )
})

test_that("plan_tech_prod and production under scenario targets are equal
          (assuming equals values before funcion is applied) if capacity factors
          do not differ between scenarios.", {
  test_baseline <- "NPS"
  test_target <- "SDS"

  test_results <- convert_power_cap_to_generation(
    data = new_test_data,
    capacity_factors_power = new_capacity_factors %>%
      dplyr::mutate(capacity_factor = 0.5),
    baseline_scenario = test_baseline,
    target_scenario = test_target
  )

  expect_equal(test_results$plan_tech_prod, test_results %>% dplyr::pull(!!rlang::sym(test_baseline)))
  expect_equal(test_results$plan_tech_prod, test_results %>% dplyr::pull(!!rlang::sym(test_target)))
})

test_that("plan_tech_prod and scen_tech_prod differ (assuming equals values before
          funcion is applied) only for power sector for not baseline scneario if
          capacity factors differ between scenarios.", {
  test_baseline <- "NPS"
  test_target <- "SDS"

  test_results <- convert_power_cap_to_generation(
    data = new_test_data,
    capacity_factors_power = new_capacity_factors,
    baseline_scenario = test_baseline,
    target_scenario = test_target
  )

  power_results_target <- test_results %>%
    dplyr::filter(ald_sector == "Power") %>%
    dplyr::select(plan_tech_prod, !!rlang::sym(test_target))
  power_results_baseline <- test_results %>%
    dplyr::filter(ald_sector == "Power") %>%
    dplyr::select(plan_tech_prod, !!rlang::sym(test_baseline))

  other_sector_results <- test_results %>% dplyr::filter(ald_sector != "Power")

  testthat::expect_true(all(power_results_target$plan_tech_prod > power_results_target %>% dplyr::pull(!!rlang::sym(test_target))))
  testthat::expect_true(all(power_results_baseline$plan_tech_prod == power_results_baseline %>% dplyr::pull(!!rlang::sym(test_baseline))))
  testthat::expect_true(all(other_sector_results$plan_tech_prod == other_sector_results %>% dplyr::pull(!!rlang::sym(test_target))))
  testthat::expect_true(all(other_sector_results$plan_tech_prod == other_sector_results %>% dplyr::pull(!!rlang::sym(test_baseline))))
})

test_that("multiplication results differ by further variables", {
  test_baseline <- "NPS"
  test_target <- "SDS"

  # picked only one year here to keep test manageable
  test_results <- convert_power_cap_to_generation(
    data = new_test_data,
    capacity_factors_power = new_capacity_factors %>%
      dplyr::mutate(capacity_factor = dplyr::if_else(.data$year == 2022, .data$capacity_factor - 0.1, .data$capacity_factor)),
    baseline_scenario = test_baseline,
    target_scenario = test_target
  )

  power_results_2021 <- test_results %>% dplyr::filter(ald_sector == "Power" & year == 2021)
  power_results_2022 <- test_results %>% dplyr::filter(ald_sector == "Power" & year == 2022)
  testthat::expect_true(all(power_results_2021$plan_tech_prod > power_results_2022$plan_tech_prod))
  testthat::expect_true(all(power_results_2021 %>% dplyr::pull(!!rlang::sym(test_target)) > power_results_2022 %>% dplyr::pull(!!rlang::sym(test_target))))
  testthat::expect_true(all(power_results_2021 %>% dplyr::pull(!!rlang::sym(test_baseline)) > power_results_2022 %>% dplyr::pull(!!rlang::sym(test_baseline))))
})
