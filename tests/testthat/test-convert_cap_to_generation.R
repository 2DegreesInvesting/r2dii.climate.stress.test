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
    dplyr::filter(technology == "NuclearCap")
  verify_capacity_factors <- test_capacity_factors %>%
    dplyr::filter(.data$technology == "NuclearCap") %>%
    dplyr::pull(.data$capacity_factor)
  hours_to_year <- 24 * 365
  verify_multiplier <- hours_to_year * verify_capacity_factors

  verify_results <- test_results %>%
    dplyr::filter(technology == "NuclearCap")

  testthat::expect_equal(
    verify_results$scen_tech_prod, verify_input_data$scen_tech_prod * verify_multiplier
  )

  testthat::expect_equal(
    verify_results$plan_tech_prod, verify_input_data$plan_tech_prod * verify_multiplier
  )
})

# convert_power_cap_to_generation -----------------------------------------
