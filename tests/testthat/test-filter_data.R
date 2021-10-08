
# filter_negative_late_and_sudden -----------------------------------------
test_that("input remains unchanged if no negative late_and_sudden levels are
          present", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "biz", "biz"),
    technology = c("some", "other", "some", "other"),
    late_and_sudden = 1:4,
    some_col = rep("sth", 4)
  )

  filtered_data <- filter_negative_late_and_sudden(input_data)

  expect_equal(input_data, filtered_data)
})

test_that("technology x company_name combinations that hold at least 1 negative
          value on late_and_sudden are removed", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = c(-1, 1, 1, 0, 1),
    some_col = rep("sth", 5)
  )

  testthat::expect_warning(filtered_data <- filter_negative_late_and_sudden(input_data), "Removed")

  expect_equal(input_data %>% dplyr::filter(!(company_name == "firm" & technology == "some")), filtered_data)
})

test_that("removal works if several company_name x technology combinations are affected", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = c(-1, 1, -1, -1, 1),
    some_col = rep("sth", 5)
  )

  testthat::expect_warning(filtered_data <- filter_negative_late_and_sudden(input_data), "Removed")

  expect_equal(input_data %>% dplyr::filter(company_name == "biz" & technology == "other"), filtered_data)
})

test_that("error is thrown if no rows remain", {
  input_data <- tibble::tibble(
    company_name = c("firm", "firm", "firm", "biz", "biz"),
    technology = c("some", "some", "other", "some", "other"),
    late_and_sudden = rep(-1, 5),
    some_col = rep("sth", 5)
  )

  expect_error(testthat::expect_warning(filtered_data <- filter_negative_late_and_sudden(input_data), "Removed"), "No rows remain")
})
