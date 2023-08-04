devtools::load_all()
library(dplyr)
library(xlsx)

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)

mlflow_uri <- "http://localhost:5000"
exp_name <- "all_scenarios_default_params_old_data"
output_dir <- file.path("sa_st_inputs_master_raw","scenar_comparison_corr")
output_excel <- "scenario_pairs_comparisons.xlsx"
dir.create(output_dir, showWarnings = FALSE)

### CODE

read_csv_from_zipped_artifacts <- function(tracking_uri, experiment_name, run_id, csv_filename) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <- mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
  f_conn <- unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
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
    tracking_uri = mlflow_uri,
    experiment_name = exp_name,
    run_id = run_id,
    csv_filename = "crispy_output.csv"
  )

  all_crispy <- dplyr::bind_rows(all_crispy, crispy)
}

all_crispy <- all_crispy %>%
  dplyr::mutate(scenario_duo = paste(baseline_scenario, "&", shock_scenario, sep = ""))

# compare_npv_roc_between_scenarios <- function(tweaked_parameter_runs_data) {
#   npv_scenarios_change_in_roc <- tweaked_parameter_runs_data %>%
#     # filtering on term as NPV stays constant accross terms
#     dplyr::filter(term == 5) %>%
#     dplyr::select(
#       scenario_duo, company_name, sector, business_unit,
#       net_present_value_baseline, net_present_value_shock
#     ) %>%
#     # compute the npv rate of change and round the result to the closest first integer
#     # As it has been observed that for some business units (eg Renewables), the
#     # NPV at shock and at baseline are very close, and gravitate around 1. While
#     # for other business units (eg Oil) the npv is far greater than 1
#     dplyr::mutate(
#       npv_rate_of_change =
#         round(net_present_value_baseline / net_present_value_shock,
#           digits = 0
#         )
#     ) %>%
#     # npv_loss_at_shock tells if the npv_rate_of_change is bigger or equal to 1
#     # If it is bigger, npv at the baseline is higher than at the shock
#     # meaning the shock induces a loss.
#     dplyr::mutate(npv_loss_at_shock = npv_rate_of_change >= 1) %>%
#     tidyr::pivot_wider(
#       id_cols = c("company_name", "sector", "business_unit"),
#       names_from = scenario_duo,
#       values_from = npv_loss_at_shock
#     )
#
#   unique_scenario_duos <- unique(tweaked_parameter_runs_data$scenario_duo)
#   n_scenario_duos <- length(unique_scenario_duos)
#
#   scenarios_npv_loss_at_shock_agreeing_rate <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
#   colnames(scenarios_npv_loss_at_shock_agreeing_rate) <- unique_scenario_duos
#   rownames(scenarios_npv_loss_at_shock_agreeing_rate) <- unique_scenario_duos
#
#   for (scenario_duo1 in unique_scenario_duos) {
#     for (scenario_duo2 in unique_scenario_duos) {
#       not_nan_scenario_duo_1 <- sum(!is.na(npv_scenarios_change_in_roc[, scenario_duo1]))
#       not_nan_scenario_duo_2 <- sum(!is.na(npv_scenarios_change_in_roc[, scenario_duo2]))
#
#       identical_loss_at_shock <- sum(
#         npv_scenarios_change_in_roc[, scenario_duo1] == npv_scenarios_change_in_roc[, scenario_duo2],
#         na.rm = TRUE
#       )
#
#       scenarios_npv_loss_at_shock_agreeing_rate[scenario_duo1, scenario_duo2] <-
#         identical_loss_at_shock / not_nan_scenario_duo_1
#       scenarios_npv_loss_at_shock_agreeing_rate[scenario_duo2, scenario_duo1] <-
#         identical_loss_at_shock / not_nan_scenario_duo_2
#     }
#   }
#   scenarios_npv_loss_at_shock_agreeing_rate
# }

compare_pd_diff_between_scenarios <- function(tweaked_parameter_runs_data) {
  pd_scenarios_change_in_diff_sign <- tweaked_parameter_runs_data %>%
    # filtering on term 5 to focus on a single pd value
    dplyr::filter(term == 5) %>%
    dplyr::select(
      scenario_duo, company_name, sector, business_unit,
      pd_baseline, pd_shock
    ) %>%
    dplyr::mutate(pd_diff = pd_baseline - pd_shock) %>%
    dplyr::mutate(pd_diff_sign = sign(pd_diff)) %>%
    tidyr::pivot_wider(
      id_cols = c("company_name", "sector", "business_unit"),
      names_from = scenario_duo,
      values_from = pd_diff_sign
    )

  unique_scenario_duos <- sort(unique(tweaked_parameter_runs_data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_pd_diff_sign_agreeing_rate <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_pd_diff_sign_agreeing_rate) <- unique_scenario_duos
  rownames(scenarios_pd_diff_sign_agreeing_rate) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      not_nan_scenario_duo_1 <- sum(!is.na(pd_scenarios_change_in_diff_sign[, scenario_duo1]))
      not_nan_scenario_duo_2 <- sum(!is.na(pd_scenarios_change_in_diff_sign[, scenario_duo2]))

      identical_pd_diff_sign <- sum(
        pd_scenarios_change_in_diff_sign[, scenario_duo1] == pd_scenarios_change_in_diff_sign[, scenario_duo2],
        na.rm = TRUE
      )

      scenarios_pd_diff_sign_agreeing_rate[scenario_duo1, scenario_duo2] <-
        identical_pd_diff_sign / not_nan_scenario_duo_1
      scenarios_pd_diff_sign_agreeing_rate[scenario_duo2, scenario_duo1] <-
        identical_pd_diff_sign / not_nan_scenario_duo_2
    }
  }

  return(scenarios_pd_diff_sign_agreeing_rate)
}

compare_npv_diff_between_scenarios <- function(tweaked_parameter_runs_data) {
  npv_scenarios_change_in_diff_sign <- tweaked_parameter_runs_data %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::select(
      scenario_duo, company_name, sector, business_unit,
      net_present_value_baseline, net_present_value_shock
    ) %>%
    dplyr::mutate(npv_diff = round(net_present_value_baseline - net_present_value_shock)) %>%
    dplyr::mutate(npv_diff=dplyr::if_else(npv_diff==0, NA, npv_diff)) %>%
    dplyr::filter(npv_diff != 0) %>%
    dplyr::mutate(npv_diff_sign = sign(npv_diff)) %>%
    tidyr::pivot_wider(
      id_cols = c("company_name", "sector", "business_unit"),
      names_from = scenario_duo,
      values_from = npv_diff_sign
    )
  unique_scenario_duos <- sort(unique(tweaked_parameter_runs_data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_diff_sign_agreeing_rate <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_diff_sign_agreeing_rate) <- unique_scenario_duos
  rownames(scenarios_npv_diff_sign_agreeing_rate) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      not_nan_scenario_duo_1 <- sum(!is.na(npv_scenarios_change_in_diff_sign[, scenario_duo1]))
      not_nan_scenario_duo_2 <- sum(!is.na(npv_scenarios_change_in_diff_sign[, scenario_duo2]))

      identical_npv_diff_sign <- sum(
        npv_scenarios_change_in_diff_sign[, scenario_duo1] == npv_scenarios_change_in_diff_sign[, scenario_duo2],
        na.rm = TRUE
      )

      scenarios_npv_diff_sign_agreeing_rate[scenario_duo1, scenario_duo2] <-
        identical_npv_diff_sign / not_nan_scenario_duo_1
      scenarios_npv_diff_sign_agreeing_rate[scenario_duo2, scenario_duo1] <-
        identical_npv_diff_sign / not_nan_scenario_duo_2
    }
  }

  return(scenarios_npv_diff_sign_agreeing_rate)
}

count_n_companies_per_duo <- function(data){
  data_npv_diff <-   data %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::select(
      scenario_duo, company_name, sector, business_unit,
      net_present_value_baseline, net_present_value_shock
    ) %>%
    dplyr::mutate(npv_diff = round(net_present_value_baseline - net_present_value_shock)) %>%
    dplyr::mutate(npv_diff=dplyr::if_else(npv_diff==0, NA, npv_diff)) %>%
    dplyr::filter(!is.na(npv_diff))

  n_companies_per_duo <- data_npv_diff %>%
    dplyr::distinct(scenario_duo, company_name, business_unit) %>%
    dplyr::group_by(scenario_duo) %>%
    dplyr::summarise(n_companies_bu=dplyr::n())
  n_companies_per_duo <- n_companies_per_duo %>% dplyr::arrange(scenario_duo)
  return(n_companies_per_duo)
}


correl_npv_diff_between_scenarios <- function(tweaked_parameter_runs_data) {
  npv_scenarios_diff <- tweaked_parameter_runs_data %>%
    # filtering on term 5 to focus on a single npv value
    dplyr::filter(term == 5) %>%
    dplyr::select(
      scenario_duo, company_name, sector, business_unit,net_present_value_difference
    )

  unique_scenario_duos <- sort(unique(tweaked_parameter_runs_data$scenario_duo))
  n_scenario_duos <- length(unique_scenario_duos)

  scenarios_npv_diff_corr_matrix <- data.frame(matrix(nrow = n_scenario_duos, ncol = n_scenario_duos))
  colnames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos
  rownames(scenarios_npv_diff_corr_matrix) <- unique_scenario_duos

  for (scenario_duo1 in unique_scenario_duos) {
    for (scenario_duo2 in unique_scenario_duos) {
      scenar1 <- npv_scenarios_diff %>% dplyr::filter(scenario_duo == scenario_duo1)
      scenar2 <- npv_scenarios_diff %>% dplyr::filter(scenario_duo == scenario_duo2)

      merge_comps <- dplyr::inner_join(scenar1 %>% dplyr::select(company_name,sector,business_unit, net_present_value_difference),
                                       scenar2 %>% dplyr::select(company_name,sector,business_unit, net_present_value_difference),
                                       by= dplyr::join_by(company_name,sector,business_unit))

      correlation <- cor(merge_comps$net_present_value_difference.x,
                         merge_comps$net_present_value_difference.y,
                         method = 'pearson')

      scenarios_npv_diff_corr_matrix[scenario_duo1, scenario_duo2] <- correlation
      scenarios_npv_diff_corr_matrix[scenario_duo2, scenario_duo1] <- correlation
    }
  }

  return(scenarios_npv_diff_corr_matrix)
}



use_duos <-
  c(
    "Oxford2021_base&Oxford2021_fast",
    "IPR2021_baseline&IPR2021_RPS",
    "IPR2021_baseline&IPR2021_FPS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_NDC&NGFS2021_REMIND_NZ2050",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS",
    "GECO2021_CurPol&GECO2021_NDC-LTS",
    "GECO2021_CurPol&GECO2021_1.5C-Unif",
    "WEO2021_APS&WEO2021_NZE_2050",
    "WEO2021_STEPS&WEO2021_NZE_2050",
    "WEO2021_APS&WEO2021_SDS",
    "WEO2021_STEPS&WEO2021_SDS"
  )



all_crispy_small <-
  all_crispy %>% dplyr::filter(scenario_duo %in% use_duos)
comparison_npv_diff <-
  correl_npv_diff_between_scenarios(all_crispy_small)
output_excel <-
  glue::glue("scenario_pairs_comparisons_general.xlsx")
write.xlsx(
  comparison_npv_diff,
  file = file.path(output_dir, output_excel),
  sheetName = "comparison_npv_diff",
  row.names = TRUE
)

for (bu in c(
  "RenewablesCap",
  "GasCap",
  "NuclearCap",
  "HydroCap",
  "Gas",
  "Oil",
  "OilCap",
  "Coal",
  "CoalCap",
  "Electric",
  "Hybrid",
  "ICE",
  "FuelCell"
)) {
  use_duos <- c("Oxford2021_base&Oxford2021_fast", "IPR2021_baseline&IPR2021_RPS",
                "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
                "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
                "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
                "WEO2021_STEPS&WEO2021_NZE_2050")

  all_crispy_small <-
    all_crispy %>% dplyr::filter(scenario_duo %in% use_duos
                                 , business_unit == bu)

  comparison_npv_diff <-
    correl_npv_diff_between_scenarios(all_crispy_small)
  # comparison_pd_diff <- compare_pd_diff_between_scenarios(all_crispy_small)
  # n_companies_per_duo <- count_n_companies_per_duo(all_crispy_small)

  output_excel <- glue::glue("scenario_pairs_comparisons_{bu}.xlsx")
  write.xlsx(
    comparison_npv_diff,
    file = file.path(output_dir, output_excel),
    sheetName = "comparison_npv_diff",
    row.names = TRUE
  )
  # write.xlsx(comparison_pd_diff, file=file.path(output_dir, output_excel), sheetName="comparison_pd_diff", append=TRUE, row.names=TRUE)
  # write.xlsx(n_companies_per_duo, file=file.path(output_dir, output_excel), sheetName="n_companies_per_duo", append=TRUE)
}
