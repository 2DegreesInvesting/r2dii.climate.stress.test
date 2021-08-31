set_global_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  grouping_variables <<- cfg$GroupingVariables

  financial_timestamp <<- cfg$TimeStamps$FinancialData.Timestamp
  if (is.null(financial_timestamp)) {
    stop("Error: No Financial Timestamp is defined in the parameter file. Please add a FinancialData.Timestamp!")
  }

  ald_timestamp <<- cfg$TimeStamps$ALD.Timestamp
  if (is.null(ald_timestamp)) {
    stop("Error: No Asset level Data Timestamp is defined in the parameter file. Please add a ALD.Timestamp!")
  }

  datastore_timestamp <<- cfg$TimeStamps$DataStore.Export

  dataprep_timestamp <<- cfg$TimeStamps$DataPrep.Timestamp
  if (is.null(dataprep_timestamp)) {
    stop("Error: No Analysis Inputs Timestamp is defined in the parameter file. Please add a dataprep_timestamp in the parameter file!")
  }

  start_year <<- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <<- cfg$AnalysisPeriod$Years.Horizon
  risk_year <<- cfg$AnalysisPeriod$Years.Riskyear
  additional_year <<- cfg$AnalysisPeriod$Years.Additional

  tech_list <<- cfg$Lists$Technology.List
  tech_exclude <<- cfg$Lists$Technology.Exclusion.List
  sector_list <<- cfg$Lists$TechnologyRoadmap.Sector.List
  other_sector_list <<- cfg$Lists$CO2Intensity.Sector.List

  scenario_sources_list <<- cfg$Lists$Scenario.Sources.List
  iea_scenario_list <<- cfg$Lists$IEA.Scenarios.List
  web_region_list <<- cfg$Lists$WebToolRegions
  scenario_geographies_list <<- cfg$Lists$Scenario.Geography.List

  equity_market_list <<- cfg$Lists$Equity.Market.List

  allowable_asset_list <<- cfg$Lists$AssetTypes
  if (is.null(allowable_asset_list)) {
    allowable_asset_list <<- c("Funds", "Equity", "Bonds", "Others")
  }
  # allowable_asset_list <<- allowable_asset_list

  global_aggregate_sector_list <<- cfg$Lists$Global.Aggregate.Sector.List
  global_aggregate_scenario_sources_list <<- cfg$Lists$Global.Aggregate.Scenario.Sources.List


  meta_investor_name <<- cfg$ComparisonBenchmarks$MetaInvestorName
  meta_portfolio_name <<- cfg$ComparisonBenchmarks$MetaPortfolioName

  inc_meta_portfolio <<- cfg$ComparisonBenchmarks$CreateMetaPortfolio
  # if(is.null(inc_metaportfolio)){
  #   inc_metaportfolio <<- FALSE
  # }


  # inc_project_metaportfolio <<- cfg$ComparisonBenchmarks$CreateProjectMetaPortfolio
  #
  # if(is.null(inc_project_metaportfolio)){
  #   inc_project_metaportfolio <<- FALSE
  # }
  # if(inc_project_metaportfolio){
  #   project_meta_investor_name <<- paste0("Project ", meta_investor_name)
  #   project_meta_portfolio_name <<- paste0("Project ", meta_portfolio_name)
  # }

  has_map <<- cfg$Methodology$HasMAP
  if (is.null(has_map)) {
    has_map <<- TRUE
    print("Warning: has_map set to standard value (TRUE) as not defined in the parameter file")
  }

  has_sb <<- cfg$Methodology$HasSB
  if (is.null(has_sb)) {
    has_sb <<- FALSE
    print("Warning: has_sb set to standard value (FALSE) as not defined in the parameter file")
  }

  has_credit <<- cfg$Methodology$HasCC
  if (is.null(has_credit)) {
    has_credit <<- FALSE
    print("Warning: has_credit set to standard value (FALSE) as not defined in the parameter file")
  }

  has_revenue <<- cfg$Methodology$HasRevenue
  if (is.null(has_revenue)) {
    has_revenue <<- TRUE
    print("Warning: has_revenue set to standard value (TRUE) as not defined in the parameter file")
  }

  inc_emission_factors <<- cfg$Methodology$IncEmissionFactors
  if (is.null(inc_emission_factors)) {
    inc_emission_factors <<- FALSE
    print("Warning: inc_emission_factors set to standard value (inc_emission_factors) as not defined in the parameter file")
  }

  file_format_list <<- tolower(cfg$data_output$file_type)
  if (is.null(file_format_list) | length(file_format_list) == 0) {
    file_format_list <<- c("rda")
    print("Warning: file_format_list set to standard value ('rda') as not defined in the parameter file")
  }
}

set_project_paths <- function(project_name, twodii_internal, project_location_ext) {
  portcheck_v2_path <<- path_dropbox_2dii("PortCheck_v2")
  project_location <<- ifelse(twodii_internal,
    path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name),
    paste0(project_location_ext, "/", project_name)
  )

  log_path <<- paste0(project_location, "/00_Log_Files")
  par_file_path <<- paste0(project_location, "/10_Parameter_File")
  raw_input_path <<- paste0(project_location, "/20_Raw_Inputs")
  proc_input_path <<- paste0(project_location, "/30_Processed_Inputs")
  results_path <<- paste0(project_location, "/40_Results")
  outputs_path <<- paste0(project_location, "/50_Outputs")
}

set_analysis_inputs_path <- function(twodii_internal, data_location_ext, dataprep_ref = DATAPREP.TIMESTAMP()) {
  if (twodii_internal) {
    analysis_inputs_path <- path_dropbox_2dii("PortCheck", "00_Data", "07_AnalysisInputs", dataprep_ref)
    analysis_inputs_path <- paste0(analysis_inputs_path, "/")
  } else {
    analysis_inputs_path <- data_location_ext
  }

  return(analysis_inputs_path)
}

first_char_up <- function(x) {
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  x
}

# Checks whether a variable is a dataframe. Considers also logicals and null values.
data_check <- function(df) {
  if (is.data.frame(df)) {
    if (nrow(df) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  } else {
    check <- FALSE
  }

  return(check)
}

# checks validity of project config
# FIXME:
# check_valid_cfg <- function(cfg) stopifnot(exists("cfg") == TRUE)
# testthat::expect_error(check_valid_cfg())
# #> Error: `check_valid_cfg()` did not throw an error.
check_valid_cfg <- function(cfg, expected_no_args = 3) {
  stopifnot(exists("cfg") == TRUE)
  stopifnot(cfg %>% class() == "list")
  stopifnot(cfg %>% length() == expected_no_args)

  stopifnot(cfg$project_name %>% is.character() == TRUE)
  stopifnot(cfg$project_internal$twodii_internal %>% is.logical() == TRUE)

  invisible(cfg)
}
