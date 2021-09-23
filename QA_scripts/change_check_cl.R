#-------------- Change checking helper

# defining some functions
import_asset_results <- function(project_name, investor_name) {
  results_path <- path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "40_Results")

  loanbook_results_company <- readr::read_csv(file.path(results_path, paste0("stress_test_results_lb_comp_", project_name, ".csv")))
  loanbook_results_port <- readr::read_csv(file.path(results_path, paste0("stress_test_results_lb_port_", project_name, ".csv")))
  loanbook_expected_loss <- readr::read_csv(file.path(results_path, paste0("stress_test_results_lb_comp_el_", project_name, ".csv")))
  loanbook_annual_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_lb_sector_pd_changes_annual.csv"))
  loanbook_overall_pd_changes_sector <- readr::read_csv(file.path(results_path, "stress_test_results_lb_sector_pd_changes_overall.csv"))

  asset_results <- list(
    loanbook_results_company = loanbook_results_company,
    loanbook_results_port = loanbook_results_port,
    loanbook_expected_loss = loanbook_expected_loss,
    loanbook_annual_pd_changes_sector = loanbook_annual_pd_changes_sector,
    loanbook_overall_pd_changes_sector = loanbook_overall_pd_changes_sector
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
## of snapshot tests for loanbook results.
## It makes sense to use whenever you expect all or some results to be unchanged
## by a release as it checks for equality of old and new data.
## If not you will have to change the expectations to use it.

### 1. check out master branch of repo (or whichever branch you want to use as reference)
source("stress_test_model_loanbook.R") # calculates results with checked out branch

### 2. run the following lines to obtain results
project_name <- cfg_st$project_name
investor_name <- "Meta Investor"

old_results <- import_asset_results(
  project_name = project_name,
  investor_name = investor_name
)

### 3. check out dev branch of repo (or whichever branch you want to use as comparison)
source("stress_test_model_loanbook.R") # calculates results with checked out branch

### 4. run the following lines to run script or equity and bonds and obtain results
new_results <- import_asset_results(
  project_name = project_name,
  investor_name = investor_name
)

### 5. run the following line to check that data remained unchanged.
check <- check_all_equal(old_results = old_results, new_results = new_results)
check
