Sys.setenv(
  MLFLOW_PYTHON_BIN="/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python",
  MLFLOW_BIN="/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"
)

# mlflow server --backend-store-uri mlruns --host 127.0.0.1 --port 5000

#devtools::install_github("mlflow/mlflow", subdir = "mlflow/R/mlflow")

library(mlflow)
#library(carrier)

#server <- mlflow::mlflow_server(file_store="mlruns", host="127.0.0.1", port=5000)

tracking_uri <- "http://127.0.0.1:5000"

mlflow::mlflow_set_tracking_uri(uri=tracking_uri)
client <- mlflow::mlflow_client(tracking_uri)

experiment_uri = "Test"
mlflow::mlflow_create_experiment(experiment_uri)
mlflow::mlflow_set_experiment(experiment_uri)

with(mlflow::mlflow_start_run(), {

  mlflow::mlflow_log_param("alpha", 1)
  mlflow::mlflow_log_param("lambda", 3)
  mlflow::mlflow_log_metric("rmse", 0)
  mlflow::mlflow_log_metric("r2", 1)
  mlflow::mlflow_log_metric("mae", 0)

  #mlflow_log_model(predictor, "model")
})
