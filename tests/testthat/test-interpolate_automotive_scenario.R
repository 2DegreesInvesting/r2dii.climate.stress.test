test_that("an empty fair_share_perc in the auto sector is interpolated", {
  test_scenario_data <- readRDS(testthat::test_path("test_data", "test_interpolate_auto.rds"))

  pre_fct_count_na_electric <- sum(is.na(test_scenario_data %>% dplyr::filter(technology == "Electric") %>% dplyr::pull(fair_share_perc)))
  post_fct_count_na_electric <- sum(is.na(test_scenario_data %>% correct_automotive_scendata() %>% dplyr::filter(technology == "Electric") %>% dplyr::pull(fair_share_perc)))

  expect_true(pre_fct_count_na_electric == 1)
  expect_true(post_fct_count_na_electric == 0)
  expect_true(pre_fct_count_na_electric > post_fct_count_na_electric)
})

test_that("a non-empty fair_share_perc in the auto sector that is in the
          interpolation_years, is interpolated", {
  test_scenario_data <- readRDS(testthat::test_path("test_data", "test_interpolate_auto.rds"))

  pre_fct_value_electric_2031 <- test_scenario_data %>%
    dplyr::filter(technology == "Electric", year == 2031) %>%
    dplyr::pull(fair_share_perc)
  pre_fct_is.na_electric_2031 <- is.na(test_scenario_data %>% dplyr::filter(technology == "Electric", year == 2031) %>% dplyr::pull(fair_share_perc)) == TRUE
  post_fct_value_electric_2031 <- test_scenario_data %>%
    correct_automotive_scendata(interpolation_years = c(2031)) %>%
    dplyr::filter(technology == "Electric", year == 2031) %>%
    dplyr::pull(fair_share_perc)

  expect_true(pre_fct_value_electric_2031 == 0)
  expect_false(pre_fct_is.na_electric_2031)
  expect_false(post_fct_value_electric_2031 == 0)
  expect_false(post_fct_value_electric_2031 == pre_fct_value_electric_2031)
})

test_that("no empty values that are not in the auto sector are interpolated", {
  test_scenario_data <- readRDS(testthat::test_path("test_data", "test_interpolate_auto.rds"))

  pre_fct_count_na_coal <- sum(is.na(test_scenario_data %>% dplyr::filter(technology == "Coal") %>% dplyr::pull(fair_share_perc)))
  post_fct_count_na_coal <- sum(is.na(test_scenario_data %>% correct_automotive_scendata() %>% dplyr::filter(technology == "Coal") %>% dplyr::pull(fair_share_perc)))

  expect_true(pre_fct_count_na_coal == 1)
  expect_true(post_fct_count_na_coal == 1)
  expect_true(pre_fct_count_na_coal == post_fct_count_na_coal)
})

test_that("no values are interpolated for years that are passed as
          interpolation_years but sectors not being in the auto sector", {
  test_scenario_data <- readRDS(testthat::test_path("test_data", "test_interpolate_auto.rds"))

  pre_fct_value_coal_2031 <- test_scenario_data %>%
    dplyr::filter(technology == "Coal", year == 2031) %>%
    dplyr::pull(fair_share_perc)
  pre_fct_is.na_coal_2031 <- is.na(test_scenario_data %>% dplyr::filter(technology == "Coal", year == 2031) %>% dplyr::pull(fair_share_perc)) == TRUE
  post_fct_value_coal_2031 <- test_scenario_data %>%
    correct_automotive_scendata(interpolation_years = c(2031)) %>%
    dplyr::filter(technology == "Coal", year == 2031) %>%
    dplyr::pull(fair_share_perc)

  expect_true(pre_fct_value_coal_2031 == 0)
  expect_false(pre_fct_is.na_coal_2031)
  expect_true(post_fct_value_coal_2031 == 0)
  expect_true(post_fct_value_coal_2031 == pre_fct_value_coal_2031)
})
