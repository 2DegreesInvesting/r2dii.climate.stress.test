test_that("is sensitive to `asset_type`, and defaults to bonds", {
  local_dev_env()
  expect_equal(run_stress_test("equity"), "equity")
})

test_that("with invalid `asset_type` errors gracefully", {
  expect_error(run_stress_test("invalid"), "should be.*bonds.*equity.*loans")
})

test_that("if `asset_type` is longer than 1 uses default with a warning", {
  local_dev_env()
  expect_snapshot(out <- run_stress_test())
  expect_equal(out, "bonds")
})

test_that("returns the first argument invisibly", {
  readRenviron(here::here(".Renviron"))

  FIXME <- "Negative absolute production is impossible"
  expect_warning(
    expect_invisible(out <- run_stress_test("bonds")),
    FIXME
  )

  expect_equal(out, "bonds")
})
