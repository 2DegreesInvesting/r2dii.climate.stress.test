#' Write stress test reports to output dir
#'
#' Stress test results are  exported to the output dir.
#'
#' @param results_list A list of st results.
#' @param iter_var String holding name of iteration variable.
#' @param output_path String holding path to output dir.
#' @param shock_scenario String holding shock scenario name.
#' @param scenario_geography String holding scenario geography name.
#' @param carbon_price_model String holding carbon price model for trisk.
#' @param risk_type String holding the risk type.
#'
#' @return NULL
write_stress_test_results <- function(results_list, iter_var, shock_scenario, scenario_geography, carbon_price_model, risk_type,
                                      output_path) {

  if (risk_type == "trisk") {
  results_list$company_trajectories %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("company_trajectories_{iter_var}_{shock_scenario}_{scenario_geography}_{carbon_price_model}.csv")
    ))
  }

  if (risk_type == "lrisk") {
    results_list$company_trajectories %>%
      readr::write_csv(file.path(
        output_path,
        glue::glue("company_trajectories_{iter_var}_{shock_scenario}_{scenario_geography}.csv")
      ))
  }


  if (risk_type == "trisk") {
  results_list$crispy_output %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("crispy_output_{iter_var}_{shock_scenario}_{scenario_geography}_{carbon_price_model}.csv")
    ))
  }

  if (risk_type == "lrisk") {
    results_list$crispy_output %>%
      readr::write_csv(file.path(
        output_path,
        glue::glue("crispy_output_{iter_var}_{shock_scenario}_{scenario_geography}.csv")
      ))
  }
}
