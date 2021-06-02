test_that("assigns the value of a field to a variable named as that field", {
  # Fake a transparent configuration file.
  fake_config <- function(text) {
    path <- tempfile()
    writeLines(text, con = path)
    path
  }

  yaml <- fake_config("
    default:
        reporting:
            project_report_name: general
            display_currency: USD
  ")
  suppressWarnings(set_project_parameters(yaml))

  expect_equal(
    display_currency,
    config::get(file = yaml)$reporting$display_currency
  )
  expect_equal(
    project_report_name,
    config::get(file = yaml)$reporting$project_report_name
  )
})
