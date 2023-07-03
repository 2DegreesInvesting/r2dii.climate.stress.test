library(dplyr)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)

exp_name <- "all_scenarios_default_params_new_data"
abcd_folder <- "~/2° Investing Dropbox/Bertrand Gallice/r2dii.climate.stress.test/new_st_inputs/project_input"


read_csv_from_zipped_artifacts <- function(tracking_uri, experiment_name, run_id, csv_filename) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
  f_conn <- unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}


mlflow::mlflow_set_tracking_uri(uri = "http://localhost:5000")
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(
    filter = "tags.LOG_STATUS = 'SUCCESS'",
    experiment_ids = as.character(experiment_id)
  )


all_crispy <- NULL
for (run_id in all_runs[["run_uuid"]]) {
  crispy <- read_csv_from_zipped_artifacts(
    tracking_uri = "http://localhost:5000",
    experiment_name = exp_name,
    run_id = run_id,
    csv_filename = "crispy_output.csv"
  )
  all_crispy <- dplyr::bind_rows(all_crispy, crispy)
}

use_prop <- TRUE
if (use_prop){
random_sel <- all_crispy %>%
  dplyr::distinct(sector, business_unit, company_name) %>%
  dplyr::group_by(sector, business_unit) %>%
  dplyr::group_modify(~ dplyr::slice_sample(.x, prop = 0.3))
} else{
  random_sel <- all_crispy %>%
    dplyr::distinct(sector, business_unit, company_name) %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::group_modify(~ dplyr::slice_sample(.x, n = 10))
}

original <- all_crispy %>%
  dplyr::distinct(sector, business_unit, company_name) %>%
  dplyr::group_by(sector, business_unit) %>%
  dplyr::summarise(n_per_bu = dplyr::n(), .groups = "drop")

# check that selection is roughly the value of prop (default 50%) for all BU
check <- dplyr::inner_join(original,
  random_sel %>%
    dplyr::group_by(sector, business_unit) %>%
    dplyr::summarise(n_per_bu = dplyr::n()),
  by = c("sector", "business_unit")
) %>%
  dplyr::mutate(prop_sample_per_bu = n_per_bu.y / n_per_bu.x)
View(check)
stopifnot(nrow(
  all_crispy %>%
    dplyr::distinct(sector, business_unit) %>%
    dplyr::anti_join(check %>% dplyr::select(sector, business_unit))
  ) == 0)

# ==========
# filter abcd input

abcd_input_path <- file.path(abcd_folder, "abcd_stress_test_input.csv")
abcd_input <- readr::read_csv(abcd_input_path)

abcd_reduced <- abcd_input %>%
  dplyr::right_join(random_sel,
    by = c(
      "company_name" = "company_name",
      "ald_sector" = "sector",
      "technology" = "business_unit"
    )
  )


readr::write_csv(abcd_reduced, file.path(abcd_folder, "abcd_stress_test_input_reduced.csv"))
