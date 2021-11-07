test_that("with no argument with multiple values errors gracefully", {
  skip_if_not(is_registered_dev())

  data <- st_data_paths()
  expect_snapshot_error(st_df(data, asset_type = "bonds", term = 1))
})

test_that("with more than one long stress-test argument errors gracefully", {
  skip_if_not(is_registered_dev())

  long1 <- range(lgd_senior_claims_range_lookup)
  expect_no_error(
    suppressWarnings(
      st_df(
        st_data_paths(),
        "bonds",
        lgd_senior_claims = long1,
        term = 1
      )
    )
  )

  long2 <- range(terminal_value_range_lookup)
  expect_snapshot_error(
    st_df(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      terminal_value = long2,
      term = 1
    )
  )
})

test_that("iterates over the long argument", {
  skip_if_not(is_registered_dev())

  long1 <- range(lgd_senior_claims_range_lookup)
  out <- suppressWarnings(
    st_df(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      term = 1
    )
  )

  expect_s3_class(out, "data.frame")

  expected <- c("st_type", "st_name", "arg_name", "arg_value")
  expect_true(all(expected %in% names(out)))
})

test_that("with partial match to `...` errors gracefully", {
  expect_error(st_bonds(st_data_paths(), term = c(1, 2)), "problem.*term")
})
