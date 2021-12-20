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
    expected_levels_list = list(
      a = c("A1"),
      b = c("B1", "B2", "B3")
    )
  ), "Expected levels are B1, B2, B3")
})
