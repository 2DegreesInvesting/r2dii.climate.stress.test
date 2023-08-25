devtools::load_all()
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(ggbeeswarm)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
           MLFLOW_BIN = mlflow_bin)

mlflow_uri <- "http://localhost:5000"
exp_name <- "cgfi_paper_new_toni_data"

### CODE

read_csv_from_zipped_artifacts <-
  function(tracking_uri,
           experiment_name,
           run_id,
           csv_filename) {
    mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
    mlflow::mlflow_client()

    artifacts_path <-
      mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
    f_conn <-
      unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
    artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
    return(artifact)
  }


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(filter = "tags.LOG_STATUS = 'SUCCESS'",
                             experiment_ids = as.character(experiment_id))


all_crispy <- NULL
for (run_id in all_runs[["run_uuid"]]) {
  crispy <- read_csv_from_zipped_artifacts(
    tracking_uri = mlflow_uri,
    experiment_name = exp_name,
    run_id = run_id,
    csv_filename = "crispy_output.csv"
  )
  crispy <- crispy %>%
    dplyr::mutate(scenario_duo = paste(baseline_scenario, "&", shock_scenario, sep = ""))
  crispy <- crispy %>%
    dplyr::mutate(run_id = run_id)

  all_crispy <- dplyr::bind_rows(all_crispy, crispy)
}

