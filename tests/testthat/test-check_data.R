# check_technology_availability -------------------------------------------
test_that("informative error is thrown if technologies are missing", {
  expect_error(check_technology_availability(
    data = tibble::tibble(technology = "A"),
    expected_technologies = c("A", "B", "C")
  ), "Missing technologies: B, C.")
})


# check_level_availability ------------------------------------------------
test_that("informative error is thrown if levels are missing", {
  expect_error(check_level_availability(
    data = tibble::tibble(
      a = c("A1", "A2"),
      b = c("B1", "B2")
    ),
    data_name = "some data",
    expected_levels_list = list(
      a = c("A1"),
      b = c("B1", "B2", "B3")
    )
  ), "Expected levels are B1, B2, B3")
})


# check_sector_tech_mapping -----------------------------------------------
test_that("input is returned if mapping is correct", {
  test_data <- tibble::tibble(
    sector = c("Oil&Gas", "Oil&Gas"),
    technology = c("Oil", "Gas")
  )

  test_data_checked <- check_sector_tech_mapping(
    data = test_data,
    sector_col = "sector"
  )

  expect_equal(test_data, test_data_checked)
})

test_that("error is thrown in case technology expected for ald_sector is missing.", {
  test_data <- tibble::tibble(
    ald_sector = c("Oil&Gas"),
    technology = c("Oil")
  )

  expect_error(check_sector_tech_mapping(
    data = test_data
  ), "Incorrect mapping")
})

test_that("error is thrown in case technology not expected for ald_sector is present.", {
  test_data <- tibble::tibble(
    ald_sector = c("Oil&Gas", "Oil&Gas", "Oil&Gas"),
    technology = c("Oil", "Gas", "&")
  )

  expect_error(check_sector_tech_mapping(
    data = test_data,
  ), "Incorrect mapping")
})
