test_that("Error is thrown if input vector have different lengths", {
  expect_error(
    calc_survival_probability_merton(
      L = c(10, 20),
      V0 = 20,
      sigma = 0.2,
      r = 0.05,
      t = 20
    ),
    "same length"
  )
})

test_that("Error is thrown if any input vector is not numeric", {
  expect_error(
    calc_survival_probability_merton(
      L = 10,
      V0 = "20",
      sigma = 0.2,
      r = 0.05,
      t = 20
    ),
    "numeric"
  )
})

test_that("Error is thrown if r is negative", {
  expect_error(
    calc_survival_probability_merton(
      L = 10,
      V0 = 1,
      sigma = 0.2,
      r = -0.05,
      t = 20
    ),
    "negative"
  )
})

test_that("Error is thrown if any input other than r is not positive", {
  expect_error(
    calc_survival_probability_merton(
      L = 10,
      V0 = 0,
      sigma = 0.2,
      r = 0,
      t = 20
    ),
    "positive"
  )
})

test_that("Function returns vector of identical length as input args", {
  p_survival <- calc_survival_probability_merton(
    L = c(10, 10),
    V0 = c(20, 20),
    sigma = c(0.2, 0.2),
    r = c(0.05, 0.05),
    t = c(1, 2)
  )

  expect_equal(length(p_survival), 2)
})

