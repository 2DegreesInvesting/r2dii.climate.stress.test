#-------------- Change checking helper

# defining some functions
import_asset_results <- function(project_name) {
  results_path <- file.path(get_st_data_path("ST_PROJECT_FOLDER"), "outputs")

  bonds_results_company <- readr::read_csv(file.path(results_path, "stress_test_results_bonds_comp.csv"))
  bonds_results_port <- readr::read_csv(file.path(results_path, "stress_test_results_bonds_port.csv"))
  bonds_expected_loss <- readr::read_csv(file.path(results_path, paste0("stress_test_results_cb_comp_el_", project_name, ".csv")))
  bonds_annual_pd_changes_sector <- readr::read_csv(file.path(results_path, paste0("stress_test_results_cb_sector_pd_changes_annual.csv")))
  bonds_overall_pd_changes_sector <- readr::read_csv(file.path(results_path, paste0("stress_test_results_cb_sector_pd_changes_overall.csv")))

  asset_results <- list(
    bonds_results_company = bonds_results_company,
    bonds_results_port = bonds_results_port,
    bonds_expected_loss = bonds_expected_loss,
    bonds_annual_pd_changes_sector = bonds_annual_pd_changes_sector,
    bonds_overall_pd_changes_sector = bonds_overall_pd_changes_sector
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
      warning(paste0("Data for ", n, " are not equal."))
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
## of snapshot tests for bond results.
## It makes sense to use whenever you expect all or some results to be unchanged
## by a release as it checks for equality of old and new data.
## If not you will have to change the expectations to use it.

### 0. set a seed at the top of stress_test_model_cb.R

### 1. check out master branch of repo (or whichever branch you want to use as reference)
devtools::load_all()
run_stress_test_bonds()

### 2. run the following lines to obtain results
project_name <- config::get(file = "st_project_settings.yml")$project_name

old_results <- import_asset_results(
  project_name = project_name
)

### 3. check out dev branch of repo (or whichever branch you want to use as comparison)
devtools::load_all()
run_stress_test_bonds()

### 4. run the following lines to run script or equity and bonds and obtain results
new_results <- import_asset_results(
  project_name = project_name
)

### 5. run the following line to check that data remained unchanged.
check <- check_all_equal(old_results = old_results, new_results = new_results)
check
