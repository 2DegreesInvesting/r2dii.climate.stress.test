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
