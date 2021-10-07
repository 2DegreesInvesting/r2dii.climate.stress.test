###########################################################################
# Project Initialisation---------------------------------------------------
###########################################################################

library(r2dii.data)
library(r2dii.match)
library(r2dii.analysis)
library(dplyr)
library(fs)
library(readr)
library(readxl)
library(purrr)
library(vroom)

source(file.path("R", "utils.R"))
source(file.path("R", "set_paths.R"))
source("stress_test_model_functions.R")
source("0_global_functions_st.R")
source(file.path("R", "format_loanbook_st.R"))


#---set up project---------
cfg_st <- config::get(file = "st_project_settings.yml")
# check_valid_cfg(cfg = cfg_st, expected_no_args = 3)
project_name <- cfg_st$project_name
year_production_data_forecast <- 2019
year_scenario_data <- 2019

###########################################################################
# Load input datasets------------------------------------------------------
###########################################################################

# loan book
loanbook <- read_csv(
  path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "20_Raw_Inputs", paste0("raw_loanbook_", project_name, ".csv")),
  col_types = cols(
    id_loan = "c",
    id_direct_loantaker = "c",
    name_direct_loantaker = "c",
    id_intermediate_parent_1 = "c",
    name_intermediate_parent_1 = "c",
    id_ultimate_parent = "c",
    name_ultimate_parent = "c",
    loan_size_outstanding = "d",
    loan_size_outstanding_currency = "c",
    loan_size_credit_limit = "d",
    loan_size_credit_limit_currency = "c",
    sector_classification_system = "c",
    sector_classification_input_type = "c",
    sector_classification_direct_loantaker = "c",
    fi_type = "c",
    flag_project_finance_loan = "c",
    name_project = "c",
    lei_direct_loantaker = "c",
    isin_direct_loantaker = "c"
  )
)

# Production forecast data
if (year_production_data_forecast == 2019) {
  production_data_forecast <- read_xlsx(
    r2dii.utils::path_dropbox_2dii(
      "2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA",
      "Banks work", "02_Documentation", "Public launch", "ready",
      "ald_15092020.xlsx"
    )
  )
} else if (year_production_data_forecast == 2020) {
  production_data_forecast <- readr::read_csv(
    r2dii.utils::path_dropbox_2dii(
      "2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA",
      "Banks work", "04_Data", "Data", "20200701_r2dii_release_data", "data",
      "ald_15092020.csv"
    )
  )
} else stop(
  glue::glue(
    "No production forecast data available for {year_production_data_forecast}"
  )
)


# Scenario data - market share
scenario_data_market_share <- readr::read_csv(
  r2dii.utils::path_dropbox_2dii(
    "2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA",
    "Banks work", "04_Data", "Data", "20200701_r2dii_release_data", "data",
    glue::glue("scenario_{year_scenario_data}.csv")
  ),
  col_types = cols(
    scenario_source = "c",
    scenario = "c",
    sector = "c",
    technology = "c",
    region = "c",
    year = "d",
    tmsr = "d",
    smsp = "d"
  )
)

# Scenario data - emission intensity
scenario_data_emissions_intensity <- readr::read_csv(
  r2dii.utils::path_dropbox_2dii(
    "2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA",
    "Banks work", "04_Data", "Data", "20200701_r2dii_release_data", "data",
    glue::glue("co2_intensity_scenario_{year_scenario_data}.csv")
  ),
  col_types = cols(
    scenario_source = "c",
    scenario = "c",
    sector = "c",
    region = "c",
    year = "d",
    emission_factor = "d",
    emission_factor_unit = "c"
  )
)

# region iso mapping
regions <- r2dii.data::region_isos


###########################################################################
# match loan book to production forecast data------------------------------
###########################################################################

#---calculate PACTA on bank loan book-------

# Create temp folder for the chunked loan book; can be changed to something permanent if required
path_processed_loanbook <- file.path("PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs")
out <- r2dii.utils::path_dropbox_2dii(path_processed_loanbook, "output")
if (!fs::dir_exists(out)) fs::dir_create(out)

# size too large to be processed in one go, split loan book in chunks and loop
# TODO: develop logic to split in chunks, maybe one chunk per 5-10k rows...
chunks <- 20
chunked_loanbook <- loanbook %>%
  dplyr::mutate(chunk = as.integer(cut(dplyr::row_number(), chunks)))


# CAUTION: expensive calculation
# run loop only when the matched files have not been created yet
# # loop over the chunks, running the matching algorithm and writing the result to out
# for (i in unique(chunked_loanbook$chunk)) {
#   # 1. Match this chunk against the entire production forecast dataset.
#   this_chunk <- dplyr::filter(chunked_loanbook, chunk == i)
#   this_result <- r2dii.match::match_name(this_chunk, production_data_forecast)
#
#   # 2. If this chunk matched nothing, move to the next chunk
#   matched_nothing <- nrow(this_result) == 0L
#   if (matched_nothing) next()
#
#   # 3. Else, save the result to a .csv file.
#   vroom::vroom_write(this_result, path(out, paste0(i, ".csv")))
# }

# You can read and combine all matched chunks
# TODO: be more explicit about col_types
matched_unpriorizized <- vroom::vroom(dir_ls(out))

# TODO: add manual validation step

# priortize based on score
matched <- matched_unpriorizized %>%
  r2dii.match::prioritize()

# store matched loan book in processed inputs
matched %>%
  readr::write_csv(
    r2dii.utils::path_dropbox_2dii(path_processed_loanbook, "matched_loan_book.csv")
  )



###########################################################################
# read in matched loan book, analyze and wrangle to ST needs---------------
###########################################################################

# required data:
# matched
# loanbook
# regions
# production_data_forecast
# scenario_data_market_share
# scenario_data_emissions_intensity

matched  <- readr::read_csv(
    r2dii.utils::path_dropbox_2dii(path_processed_loanbook, "matched_loan_book.csv"),
    col_types = cols_only(
      id_loan = "c",
      id_direct_loantaker = "c",
      name_direct_loantaker = "c",
      id_intermediate_parent_1 = "c",
      name_intermediate_parent_1 = "c",
      id_ultimate_parent = "c",
      name_ultimate_parent = "c",
      loan_size_outstanding = "d",
      loan_size_outstanding_currency = "c",
      loan_size_credit_limit = "d",
      loan_size_credit_limit_currency = "c",
      sector_classification_system = "c",
      sector_classification_input_type = "c",
      sector_classification_direct_loantaker = "c",
      fi_type = "c",
      flag_project_finance_loan = "c",
      name_project = "c",
      lei_direct_loantaker = "c",
      isin_direct_loantaker = "c",
      # chunk = "d", NOT REQUIRED!
      id_2dii = "c",
      level = "c",
      sector = "c",
      sector_ald = "c",
      name = "c",
      name_ald = "c",
      score = "d",
      source = "c",
      borderline = "l"
    )
  )


# TODO: what to do with negative credit limits?
matched_non_negative <- matched %>%
  dplyr::mutate(
    loan_size_outstanding = dplyr::if_else(
      .data$loan_size_outstanding < 0, 0, .data$loan_size_outstanding
    ),
    loan_size_credit_limit = dplyr::if_else(
      .data$loan_size_credit_limit < 0, 0, .data$loan_size_credit_limit
    )
  )

# portfolio_size <- matched %>%
portfolio_size <- loanbook %>%
  # TODO: why distinct?
  dplyr::distinct(
    .data$id_loan, .data$loan_size_outstanding, .data$loan_size_credit_limit
  ) %>%
  dplyr::summarise(
    portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
    portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  )

matched_portfolio_size <- matched_non_negative %>%
  # TODO: why distinct?
  dplyr::distinct(
    .data$id_loan, .data$loan_size_outstanding, .data$loan_size_credit_limit
  ) %>%
  dplyr::summarise(
    matched_portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
    matched_portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  )


#-Calculate loan-tech level loan book size and value share------------

loan_share <- matched_non_negative %>%
  dplyr::mutate(
    portfolio_loan_size_outstanding = portfolio_size$portfolio_loan_size_outstanding,
    portfolio_loan_size_credit_limit = portfolio_size$portfolio_loan_size_credit_limit,
    matched_portfolio_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
    matched_portfolio_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE)
  ) %>%
  #TODO: either name or name_ald.. is id_2dii relevant?
  dplyr::group_by(
    .data$name_ald, .data$sector_ald, .data$loan_size_outstanding_currency,
    .data$loan_size_credit_limit_currency
  ) %>%
  dplyr::mutate(
    comp_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / .data$portfolio_loan_size_outstanding,
    comp_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
    comp_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / .data$portfolio_loan_size_credit_limit,
    comp_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    # .data$id_2dii,
    # .data$name,
    .data$name_ald,
    .data$sector_ald,
    .data$comp_loan_share_outstanding,
    .data$comp_loan_size_outstanding,
    .data$loan_size_outstanding_currency,
    .data$comp_loan_share_credit_limit,
    .data$comp_loan_size_credit_limit,
    .data$loan_size_credit_limit_currency
  ) %>%
  # TODO why distinct_all?
  dplyr::distinct_all()

loan_share %>%
  readr::write_csv(
    r2dii.utils::path_dropbox_2dii(
      "PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs",
      glue::glue("comp_overview_{project_name}.csv")
    )
  )


#-Calculate tech level loan book size and value share------------

# CALCULATE TECH LEVEL SHARES OF LOANBOOK SIZE
# TODO: figure out a way to get tech share

#-Calculate sector level loan book size and value share------------

sector_share <- matched_non_negative %>%
  dplyr::group_by(
    .data$sector_ald, .data$loan_size_outstanding_currency,
    .data$loan_size_credit_limit_currency
  ) %>%
  dplyr::summarise(
    sector_loan_share_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE) / portfolio_size$portfolio_loan_size_outstanding,
    sector_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
    sector_loan_share_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE) / portfolio_size$portfolio_loan_size_credit_limit,
    sector_loan_size_credit_limit = sum(.data$loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(
    .data$sector_ald,
    .data$sector_loan_share_outstanding,
    .data$sector_loan_size_outstanding,
    .data$loan_size_outstanding_currency,
    .data$sector_loan_share_credit_limit,
    .data$sector_loan_size_credit_limit,
    .data$loan_size_credit_limit_currency
  )

sector_share %>%
  readr::write_csv(
    r2dii.utils::path_dropbox_2dii(
      "PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs",
      glue::glue("portfolio_overview_{project_name}.csv")
    )
  )


#----Calculate comp level scenario targets--------------------

matched_company_weighted <- matched_non_negative %>%
  r2dii.analysis::target_market_share(
    ald = production_data_forecast,
    scenario = scenario_data_market_share,
    region_isos = regions,
    use_credit_limit = TRUE,
    by_company = TRUE,
    weight_production = TRUE
  ) %>%
  # TODO filter must be generalised for diff scenario inputs
  dplyr::filter(
    (.data$sector == "automotive" & .data$scenario_source == "etp_2017") |
      (.data$sector == "coal" & .data$scenario_source == "weo_2019") |
      (.data$sector == "oil and gas" & .data$scenario_source == "weo_2019") |
      (.data$sector == "power" & .data$scenario_source == "weo_2019")
  ) %>%
  dplyr::rename(
    production_weighted = .data$production
  ) %>%
  dplyr::mutate(technology_share = round(.data$technology_share, 8)) %>% # rounding errors can lead to duplicates
  # TODO: why distinct_all?
  dplyr::distinct_all()

matched_company_unweighted <- matched_non_negative %>%
  r2dii.analysis::target_market_share(
    ald = production_data_forecast,
    scenario = scenario_data_market_share,
    region_isos = regions,
    use_credit_limit = TRUE,
    by_company = TRUE,
    weight_production = FALSE
  ) %>%
  # TODO filter must be generalised for diff scenario inputs
  dplyr::filter(
    (.data$sector == "automotive" & .data$scenario_source == "etp_2017") |
      (.data$sector == "coal" & .data$scenario_source == "weo_2019") |
      (.data$sector == "oil and gas" & .data$scenario_source == "weo_2019") |
      (.data$sector == "power" & .data$scenario_source == "weo_2019")
  ) %>%
  dplyr::rename(
    production_unweighted = .data$production
  ) %>%
  dplyr::select(-.data$technology_share) %>%
  # TODO: why distinct_all?
  dplyr::distinct_all()

matched_company <- matched_company_weighted %>%
  dplyr::inner_join(
    matched_company_unweighted,
    by = c("sector", "technology", "year", "region", "scenario_source", "name_ald", "metric")
  ) %>%
  dplyr::distinct_all()

matched_company_loan_share <- matched_company %>%
  # TODO why left_join?
  dplyr::left_join(loan_share, by = c("sector" = "sector_ald", "name_ald")) %>%
  dplyr::filter(.data$region == "global") %>%
  dplyr::select(
    -c(
      .data$comp_loan_size_outstanding, .data$comp_loan_size_credit_limit,
      .data$loan_size_outstanding_currency, .data$loan_size_credit_limit_currency
      )
    ) %>%
  dplyr::rename(
    loan_share_outstanding = .data$comp_loan_share_outstanding,
    loan_share_credit_limit = .data$comp_loan_share_credit_limit
  )

# Write to results
matched_company_loan_share %>%
  readr::write_csv(
    r2dii.utils::path_dropbox_2dii(
      "PortCheck_v2", "10_Projects", project_name, "40_Results",
      glue::glue("company_results_lb_{project_name}.csv")
    )
  )

