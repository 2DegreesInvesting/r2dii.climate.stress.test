#----initiate---------

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
check_valid_cfg(cfg = cfg_st, expected_no_args = 3)
project_name <- cfg_st$project_name


#---load required data sets----------

# loan book
loanbook <- read_csv(
  path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "20_Raw_Inputs", paste0("loanbook_demo_", project_name, ".csv")),
  col_types = "cccccccncncccncclll"
)

# Asset level data
ald <- read_xlsx(
  path_dropbox_2dii("2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA . Regulator Monitoring", "Banks work", "02_Documentation", "Public launch", "ready", "ald_15092020.xlsx")
)

# Scenario data - market share
scenario_data_market_share <- read_csv(
  path_dropbox_2dii("2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA . Regulator Monitoring", "Banks work", "02_Documentation", "Public launch", "ready", "scenario_2020_FF_Power_Auto.csv"),
  col_types = "cccccddd"
)

# Scenario data - emission intensity
# scenario_data_emissions <- read_csv(
#   path_dropbox_2dii("2° Investing Team", "1. RESEARCH", "1. Studies (projects)", "PACTA . Regulator Monitoring", "Banks work", "02_Documentation", "Public launch", "ready", "co2_intensity_scenario_2020_Steel_Cement.csv"),
#   col_types = "ccccddc"
# )

regions <- r2dii.data::region_isos


#---calculate PACTA on bank loan book-------

# Create temp folder for the chunked loan book; can be changed to something permanent if required
path_processed_loanbook <- file.path("PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs")
out <- path(path_dropbox_2dii(path_processed_loanbook), "output")
if (!dir_exists(out)) dir_create(out)

# size too large to be processed in one go, split loan book in chunks and loop
# TODO: develop logic to split in chunks, maybe one chunk per 5-10k rows...
chunks <- 20
chunked_loanbook <- loanbook %>% dplyr::mutate(chunk = as.integer(cut(row_number(), chunks)))


# CAUTION: expensive calculation
# run loop only when the matched files have not been created yet
# # loop over the chunks, running the matching algorithm and writing the result to out
# for (i in unique(chunked_loanbook$chunk)) {
#   # 1. Match this chunk against the entire ald dataset.
#   this_chunk <- filter(chunked_loanbook, chunk == i)
#   this_result <- match_name(this_chunk, ald)
#
#   # 2. If this chunk matched nothing, move to the next chunk
#   matched_nothing <- nrow(this_result) == 0L
#   if (matched_nothing) next()
#
#   # 3. Else, save the result to a .csv file.
#   vroom_write(this_result, path(out, paste0(i, ".csv")))
# }

# You can read and combine all matched chunks
# TODO: be more explicit about col_types
matched_unpriorizized <- vroom(dir_ls(out))

# TODO: add manual validation step

# priortize based on score
matched <- matched_unpriorizized %>%
  prioritize()

# TODO: what to do with negative credit limits?
matched_non_negative <- matched %>%
  dplyr::mutate(
    loan_size_outstanding = ifelse(loan_size_outstanding < 0, 0, loan_size_outstanding),
    loan_size_credit_limit = ifelse(loan_size_credit_limit < 0, 0, loan_size_credit_limit)
  )

# portfolio_size <- matched %>%
portfolio_size <- loanbook %>%
  dplyr::distinct(id_loan, loan_size_outstanding, loan_size_credit_limit) %>%
  dplyr::summarise(
    portfolio_loan_size_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    portfolio_loan_size_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  )

matched_portfolio_size <- matched_non_negative %>%
  dplyr::distinct(id_loan, loan_size_outstanding, loan_size_credit_limit) %>%
  dplyr::summarise(
    matched_portfolio_loan_size_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    matched_portfolio_loan_size_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  )


#-Calculate loan-tech level loan book size and value share------------

loan_share <- matched_non_negative %>%
  dplyr::mutate(
    portfolio_loan_size_outstanding = portfolio_size$portfolio_loan_size_outstanding,
    portfolio_loan_size_credit_limit = portfolio_size$portfolio_loan_size_credit_limit,
    matched_portfolio_loan_size_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    matched_portfolio_loan_size_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE)
  ) %>%
  #TODO: either name or name_ald.. is id_2dii relevant?
  dplyr::group_by(name_ald, sector_ald, loan_size_outstanding_currency, loan_size_credit_limit_currency) %>%
  dplyr::mutate(
    comp_loan_share_outstanding = sum(loan_size_outstanding, na.rm = TRUE)/portfolio_loan_size_outstanding,
    comp_loan_size_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    comp_loan_share_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE)/portfolio_loan_size_credit_limit,
    comp_loan_size_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    # id_2dii,
    # name,
    name_ald,
    sector_ald,
    comp_loan_share_outstanding,
    comp_loan_size_outstanding,
    loan_size_outstanding_currency,
    comp_loan_share_credit_limit,
    comp_loan_size_credit_limit,
    loan_size_credit_limit_currency
  ) %>%
  dplyr::distinct_all()

loan_share %>%
  write_csv(path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs", paste0("comp_overview_", project_name, ".csv")))


#-Calculate tech level loan book size and value share------------

# CALCULATE TECH LEVEL SHARES OF LOANBOOK SIZE
# TODO: figure out a way to get tech share

#-Calculate sector level loan book size and value share------------

sector_share <- matched_non_negative %>%
  dplyr::group_by(sector_ald, loan_size_outstanding_currency, loan_size_credit_limit_currency) %>%
  dplyr::summarise(
    sector_loan_share_outstanding = sum(loan_size_outstanding, na.rm = TRUE)/portfolio_size$portfolio_loan_size_outstanding,
    sector_loan_size_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    sector_loan_share_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE)/portfolio_size$portfolio_loan_size_credit_limit,
    sector_loan_size_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(
    sector_ald,
    sector_loan_share_outstanding,
    sector_loan_size_outstanding,
    loan_size_outstanding_currency,
    sector_loan_share_credit_limit,
    sector_loan_size_credit_limit,
    loan_size_credit_limit_currency
  )

sector_share %>%
  write_csv(path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "30_Processed_Inputs", paste0("portfolio_overview_", project_name, ".csv")))


#----Calculate comp level scenario targets--------------------

matched_company_weighted <- matched_non_negative %>%
  target_market_share(
    ald = ald,
    scenario = scenario_data_market_share,
    region_isos = regions,
    use_credit_limit = TRUE,
    by_company = TRUE,
    weight_production = TRUE
  ) %>%
  dplyr::filter(
    (sector == "automotive" & scenario_source == "etp_2017") |
      (sector == "coal" & scenario_source == "weo_2019") |
      (sector == "oil and gas" & scenario_source == "weo_2019") |
      (sector == "power" & scenario_source == "weo_2019")
  ) %>%
  dplyr::rename(
    production_weighted = production
  ) %>%
  dplyr::mutate(technology_share = round(technology_share, 8)) %>% # rounding errors can lead to duplicates
  dplyr::distinct_all()

matched_company_unweighted <- matched_non_negative %>%
  target_market_share(
    ald = ald,
    scenario = scenario_data_market_share,
    region_isos = regions,
    use_credit_limit = TRUE,
    by_company = TRUE,
    weight_production = FALSE
  ) %>%
  dplyr::filter(
    (sector == "automotive" & scenario_source == "etp_2017") |
      (sector == "coal" & scenario_source == "weo_2019") |
      (sector == "oil and gas" & scenario_source == "weo_2019") |
      (sector == "power" & scenario_source == "weo_2019")
  ) %>%
  dplyr::rename(
    production_unweighted = production
  ) %>%
  dplyr::select(-technology_share) %>%
  dplyr::distinct_all()

matched_company <- matched_company_weighted %>%
  dplyr::inner_join(
    matched_company_unweighted,
    by = c("sector", "technology", "year", "region", "scenario_source", "name_ald", "metric")
  ) %>%
  dplyr::distinct_all()

matched_company_loan_share <- matched_company %>%
  dplyr::left_join(loan_share, by = c("sector" = "sector_ald", "name_ald")) %>%
  dplyr::filter(region == "global") %>%
  dplyr::select(
    -c(
      comp_loan_size_outstanding, comp_loan_size_credit_limit,
      loan_size_outstanding_currency, loan_size_credit_limit_currency
      )
    ) %>%
  dplyr::rename(
    loan_share_outstanding = comp_loan_share_outstanding,
    loan_share_credit_limit = comp_loan_share_credit_limit
  )

# Write to results
matched_company_loan_share %>%
  write_csv(path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name, "40_Results", paste0("company_results_lb_", project_name, ".csv")))

