# check_technology_availability -------------------------------------------
test_that("informative error is thrown if technologies are missing", {
  expect_error(check_technology_availability(
    data = tibble::tibble(technology = "A"),
    expected_technologies = c("A", "B", "C")
  ), "Missing technologies: B, C.")
})
