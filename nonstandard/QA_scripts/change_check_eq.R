#-------------- Change checking helper

# defining some functions
import_asset_results <- function() {

  results_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER"), "outputs")

  equity_results_company <- readr::read_csv(file.path(results_path, "stress_test_results_equity_comp_standard.csv"))
  equity_results_port <- readr::read_csv(file.path(results_path, "stress_test_results_equity_port_standard.csv"))
  equity_expected_loss <- readr::read_csv(file.path(results_path, "stress_test_results_equity_comp_el_standard.csv"))
  equity_annual_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_equity_sector_pd_changes_annual_standard.csv"))
  equity_overall_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_equity_sector_pd_changes_overall_standard.csv"))


  asset_results <- list(
    equity_results_company = equity_results_company,
    equity_results_port = equity_results_port,
    equity_expected_loss = equity_expected_loss,
    equity_annual_pd_changes_sector = equity_annual_pd_changes_sector,
    equity_overall_pd_changes_sector = equity_overall_pd_changes_sector
  )

  return(asset_results)
}

check_all_equal <- function(old_results, new_results) {

  if (!all.equal(names(old_results), names(new_results))) {
    stop("Incompatible result sets")
  }

  result_names <- names(old_results)

  check_results <- lapply(result_names, function(n) {

    old_result <- get(n, old_results)
    new_result <- get(n, new_results)

    if (!dplyr::setequal(old_result, new_result)) {
      warning(paste0("Data for ", n, " are not equal."), call. = FALSE)
      list(FALSE) %>%
        purrr::set_names(n)
    } else {
      list(TRUE) %>%
        purrr::set_names(n)
    }
  })
  return(check_results)
}


## The following script shall serve as a quick alternative to the implementation
## of snapshot tests for equity results.
## It makes sense to use whenever you expect all or some results to be unchanged
## by a release as it checks for equality of old and new data.
## If not you will have to change the expectations to use it.

### 1. check out master branch of repo (or whichever branch you want to use as reference)
devtools::load_all()
run_stress_test(
  asset_type = "equity",
  input_path_project_agnostic = get_st_data_path(),
  input_path_project_specific = get_st_data_path("ST_PROJECT_FOLDER_INPUT"),
  output_path = get_st_data_path("ST_PROJECT_FOLDER_OUTPUT")
)

### 2. run the following lines to obtain results
old_results <- import_asset_results()

### 3. check out dev branch of repo (or whichever branch you want to use as comparison)
devtools::load_all()
run_stress_test(
  asset_type = "equity",
  input_path_project_agnostic = get_st_data_path(),
  input_path_project_specific = get_st_data_path("ST_PROJECT_FOLDER_INPUT"),
  output_path = get_st_data_path("ST_PROJECT_FOLDER_OUTPUT")
)

### 4. run the following lines to run script or equity and bonds and obtain results
new_results <- import_asset_results()

### 5. run the following line to check that data remained unchanged.
check <- check_all_equal(old_results = old_results, new_results = new_results)
check
