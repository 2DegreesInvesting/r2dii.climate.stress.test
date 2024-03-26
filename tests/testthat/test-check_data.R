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

