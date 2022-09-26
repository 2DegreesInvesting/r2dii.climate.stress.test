#' Write stress test reports to output dir
#'
#' Stress test results are  exported to the output dir.
#'
#' @param results_list A list of st results.
#' @param iter_var String holding name of iteration variable.
#' @param output_path String holding path to output dir.
#'
#' @return NULL
write_stress_test_results <- function(results_list, iter_var,
                                      output_path) {
  results_list$company_trajectories %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("company_trajectories_{iter_var}.csv")
    ))

  results_list$crispy_output %>%
    readr::write_csv(file.path(
      output_path,
      glue::glue("crispy_output_{iter_var}.csv")
    ))
}
