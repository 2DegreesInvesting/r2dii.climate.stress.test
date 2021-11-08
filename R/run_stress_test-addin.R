#' Run `run_stress_test()`
#'
#' @description `run_stress_test_addin()` opens an [RStudio
#'   gadget](https://shiny.rstudio.com/articles/gadgets.html) and
#'   [addin](http://rstudio.github.io/rstudioaddins/) that allows you to set the
#'   paths to inputs and outputs. Appears as "Run stress test" in the RStudio
#'   Addins menu.
#'
#' @export
run_stress_test_addin <- function() { # nocov start
  rlang::check_installed(
    c("shiny", "miniUI"),
    "in order to use the reprex addin"
  )

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      shiny::p(
        "Run",
        shiny::a(href = "https://2degreesinvesting.github.io/r2dii.climate.stress.test/reference/run_stress_test.html", "stress test")
      ),
      right = miniUI::miniTitleBarButton("done", "Run", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::textInput("agnostic", "Folder with project-agnostic data", placeholder = "e.g. /home/you/ST_INPUTS_MASTER"),
      shiny::textInput("specific", "Folder with project-specific data", placeholder = "e.g. /home/you/ST_TESTING_BONDS"),
      shiny::selectInput("asset_type", "asset_type", choices = c("bonds", "equity", "loans")),
      st_slider("lgd_senior_claims"),
      st_slider("lgd_subordinated_claims"),
      st_slider("terminal_value"),
      st_slider("risk_free_rate"),
      st_slider("discount_rate"),
      st_slider("div_netprofit_prop_coef"),
      st_slider("shock_year"),
      st_slider("term"),
      shiny::checkboxInput("company_exclusion", "company_exclusion", value = TRUE)
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      shiny::stopApp(
        local({
          path <- c(input$agnostic, input$specific)
          name <- c("ST_DATA_PATH", "ST_PROJECT_FOLDER")
          vars <- rlang::set_names(path, name)
          withr::local_envvar(vars)

          run_stress_test(
            asset_type = input$asset_type,
            lgd_senior_claims = input$lgd_senior_claims,
            lgd_subordinated_claims = input$lgd_subordinated_claims,
            terminal_value = input$terminal_value,
            risk_free_rate = input$risk_free_rate,
            discount_rate = input$discount_rate,
            div_netprofit_prop_coef = input$div_netprofit_prop_coef,
            shock_year = input$shock_year,
            term = input$term,
            company_exclusion = input$company_exclusion
          )
        })
      )
    })
  }

  app <- shiny::shinyApp(ui, server, options = list(quiet = TRUE))
  shiny::runGadget(app, viewer = shiny::dialogViewer("Run stress test"))
}

st_slider <- function(x = "lgd_senior_claims") {
  attached <- any("package:r2dii.climate.stress.test" %in% search())
  if (!attached) {
    abort(c(
      "The package r2dii.climate.stress.test must be attached.",
      i = "Did you forget to run `library(r2dii.climate.stress.test)`?"
    ))
  }
  x_range_lookup <- paste0(x, "_range_lookup")
  x_range <- get(x_range_lookup, "package:r2dii.climate.stress.test")
  shiny::sliderInput(
    x,
    x,
    min = x_range[[1]],
    max = x_range[[2]],
    value = formals(run_stress_test)[[x]]
  )
}
