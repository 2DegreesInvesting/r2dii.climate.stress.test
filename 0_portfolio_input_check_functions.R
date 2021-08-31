### Fin data cleaning functions
map_security_sectors <- function(fin_data, sector_bridge){

  initial_no_rows = nrow(fin_data)

  fin_data <- fin_data %>% left_join(sector_bridge %>% filter(source == "BICS") %>% select(-source),
                                     by = c("security_bics_subgroup" = "industry_classification")) %>%
    mutate(security_icb_subsector = as.character(security_icb_subsector))

  fin_data_na <- fin_data %>% filter(is.na(sector)) %>% select(-c(sector,sector_boe,sector_ipr,subsector_ipr,sector_dnb,subsector_boe))

  fin_data <- fin_data %>% filter(!is.na(sector))

  fin_data_na <- fin_data_na %>% left_join(sector_bridge %>% filter(source == "ICB")%>% select(-source),
                                           by = c("security_icb_subsector" = "industry_classification"))

  fin_data <- fin_data %>% bind_rows(fin_data_na)

  fin_data <- fin_data %>% select(-security_mapped_sector,sector_boe,sector_ipr,subsector_ipr,sector_dnb,subsector_boe) %>% rename(security_mapped_sector = sector)

  fin_data %>% group_by(security_mapped_sector) %>% filter(is.na(security_mapped_sector)) %>% summarise(count = n(), .groups = "drop_last")
  fin_data_na <- fin_data %>% filter(is.na(security_mapped_sector))

  if(nrow(fin_data) != initial_no_rows){stop("Rows being dropped in mapping sectors")}

  return(fin_data)

}

override_sector_classification <- function(fin_data, overrides){

  start_rows <- nrow(fin_data)

  overrides <- overrides %>%
    mutate(across(c(company_name, corporate_bond_ticker, fin_sector_override), as.character))

  overrides$sector_override <- TRUE


  # Merge in by company corp ticker
  overrides_cbt <- overrides %>%
    filter(corporate_bond_ticker != "" , !is.na(corporate_bond_ticker)) %>%
    select(corporate_bond_ticker, fin_sector_override, sector_override) %>%
    distinct()

  fin_data <- left_join(fin_data, overrides_cbt, by = "corporate_bond_ticker")

  # Merge in by bloomberg_id
  overrides_bbg <- overrides %>%
    filter(is.na(corporate_bond_ticker)|corporate_bond_ticker == "")%>%
    select(bloomberg_id, fin_sector_override, sector_override) %>%
    distinct()

  fin_data <- left_join(fin_data, overrides_bbg, by = "bloomberg_id")


  # Clean resulting financial data
  fin_data <- fin_data %>%
    mutate(sector_override = sector_override.x,
           sector_override = if_else(sector_override.y != ""&!is.na(sector_override.y), sector_override.y, sector_override),
           fin_sector_override = fin_sector_override.x,
           fin_sector_override = if_else(!is.na(fin_sector_override.y)&fin_sector_override.y != "", fin_sector_override.y, fin_sector_override),
           sector_override = if_else(is.na(sector_override),FALSE,TRUE)) %>%
    select(-sector_override.x, -sector_override.y, -fin_sector_override.x, -fin_sector_override.y)

  fin_data <- fin_data %>%
    mutate(security_mapped_sector = if_else(sector_override, fin_sector_override, security_mapped_sector)) %>%
    select(-fin_sector_override)

  if (nrow(fin_data) != start_rows){stop("Additional rows being added by fin sector override")}

  fin_data

}

check_asset_types <- function(fin_data){

  fin_data <- fin_data %>%
    mutate(asset_type = if_else(asset_type == "Other", "Others", asset_type),
           asset_type = if_else(is.na(asset_type), "Others", asset_type),
    )

  fin_data$asset_type <- first_char_up(fin_data$asset_type)

  ### TEST
  if (!any(unique(fin_data$asset_type) %in% allowable_asset_list)){
    stop("Check Financial Data Asset Types")
  }

  fin_data

}

check_mapped_assets_flag <- function(fin_data){


  # convert old naming of "mapped to assets" column to be mapped_to_assets

  if("EQ.mapped_to_assets" %in% colnames(fin_data) | "CB.mapped_to_assets" %in% colnames(fin_data) | "has_prod_after_2018" %in% colnames(fin_data)) {

    if ("EQ.mapped_to_assets" %in% colnames(fin_data)| "CB.mapped_to_assets" %in% colnames(fin_data)){
      fin_data <- fin_data %>%
        mutate(
          mapped_to_assets = case_when(Asset.Type == "Equity" ~ EQ.mapped_to_assets,
                                       Asset.Type == "Bonds" ~ CB.mapped_to_assets,
                                       TRUE ~ 0)) %>%
        select(-CB.mapped_to_assets,-EQ.mapped_to_assets)
    }else if("has_prod_after_2018" %in% colnames(fin_data)){
      fin_data <- fin_data %>%
        mutate(
          mapped_to_assets = has_prod_after_2018
        ) %>% select(-has_prod_after_2018)
    }

  }

  unique(fin_data$mapped_to_assets)

  # Ensure that flag is a logical

  fin_data <- fin_data %>%
    mutate(mapped_to_assets = case_when(mapped_to_assets %in% c("t",1) ~ TRUE,
                                        mapped_to_assets %in% c("f",0) ~ FALSE
    ))

  ### TEST
  any(!fin_data$mapped_to_assets %in% c(TRUE, FALSE))
  ###


  fin_data


}

check_fin_mapped_sectors <- function(fin_data){

  fin_data <- fin_data %>%
    mutate(security_mapped_sector = case_when(security_mapped_sector == "Others" ~ "Other",
                                              security_mapped_sector == "OIl&Gas" ~ "Oil&Gas",
                                              is.na(security_mapped_sector) ~ "Other",
                                              TRUE ~ security_mapped_sector))

  actual_sectors <- unique(fin_data$security_mapped_sector)

  if(any(!actual_sectors %in% c(sector_list, other_sector_list, "Other"))){
    stop("Additional Sectors in fin_data")
  }

  fin_data

}

convert_corporate_bonds <- function(fin_data){

  cb_groups <- c("Convertible Bonds", "Corporate Bonds", "Corporate inflation linked Bonds","Convertible Preferreds" )

  fin_data <- fin_data %>%
    mutate(asset_type = if_else(security_type %in% cb_groups,"Bonds",asset_type),
           asset_type = if_else(!security_type %in% cb_groups & asset_type == "Bonds","Others",asset_type),
    )

  fin_data
}

identify_sb <- function(fin_data){

  sb_groups <- c("Sovereign Debt","Sovereign Agency Debt", "Government inflation linked Bonds", "Sovereign","Sovereign Agency", "Sovereigns")

  fin_data <- fin_data %>%
    mutate(is_sb = case_when(security_type %in% sb_groups ~ TRUE,
                             security_bics_subgroup %in% sb_groups ~ TRUE,
                             TRUE ~ FALSE))

  fin_data

}

classify_all_funds <- function(fin_data){

  nrow(fin_data[fin_data$asset_type == "Funds",])

  fin_data <- fin_data %>%
    mutate(asset_type = case_when(grepl("Fund", security_type) ~ "Funds" ,
                                  grepl("ETF", security_type) ~ "Funds",
                                  grepl("Fund", security_bclass4) ~ "Funds" ,
                                  grepl("ETF", security_bclass4) ~ "Funds",
                                  grepl("Fund", security_icb_subsector) ~ "Funds" ,
                                  grepl("ETF", security_icb_subsector) ~ "Funds",
                                  TRUE ~ asset_type))


  ### TEST?

  fin_data
}

### Portfolio Check Functions
check_funds_wo_bbg <- function(fund_data, fin_data){

  # isin in the fund_data but no bbg data available
  fin_data_funds <- fin_data %>% filter(asset_type == "Funds") %>% select(isin) %>% distinct()

  fund_isins <- fund_data %>% select(fund_isin) %>% distinct()

  fund_isins_missing_bbg <- fund_isins %>% filter(!fund_isin %in% fin_data_funds$isin)

  known_missing_isins <- read_csv("data-raw/fund_isins_without_bbg_data.csv", col_types =  "c")

  known_missing_isins <- known_missing_isins %>% bind_rows(fund_isins_missing_bbg) %>% distinct()

  write.csv(fund_isins_missing_bbg, "data-raw/fund_isins_without_bbg_data.csv", row.names = F)

  if (data_check(fund_isins_missing_bbg)){print("Warning: There are funds without bbg data. These are excluded from the analysis.")}

}

###
check_bloomberg_data <- function(portfolio_total){

  portfolio_total <- portfolio_total %>%
    mutate(has_bbg_data = case_when(
      (asset_type == "Equity" | asset_type == "Unclassifiable") & (is.na(bloomberg_id) | bloomberg_id == "") ~ FALSE,
      (asset_type == "Bonds" | asset_type == "Unclassifiable") & (is.na(corporate_bond_ticker) | corporate_bond_ticker == "") ~ FALSE,
      (asset_type == ""  | asset_type == "Unclassifiable") ~ FALSE,
      is.na(asset_type) ~ FALSE,
      TRUE ~ TRUE)
    )

  portfolio_total

}

add_flags <- function(portfolio){

  portfolio <- portfolio %>%
    mutate(flag = case_when(

      !has_currency ~ "Missing currency information",
      !has_valid_input ~ "Negative or missing input value",
      !has_valid_isin ~ "Invalid or missing ISIN",
      !has_bbg_data ~ "Holding not in Bloomberg database",

      TRUE ~ "Included in analysis"
    ))

  portfolio

}

overall_validity_flag <- function(portfolio_total){

  portfolio_total <- portfolio_total %>%
    mutate(valid_input = case_when(
      !has_currency ~ FALSE,
      !has_bbg_data ~ FALSE,
      !has_valid_input ~ FALSE,
      !has_valid_isin ~ FALSE,
      TRUE ~ TRUE

    ))

  portfolio_total

}

check_for_ald <- function(portfolio_subset, portfolio_type, relevant_fin_data){

  if (data_check(portfolio_subset)){
    initial_port_value = sum(portfolio_subset$value_usd, na.rm = T)

    if(portfolio_type == "Equity"){joining_id = "company_id"}else if(portfolio_type == "Bonds"){joining_id = "corporate_bond_ticker"}

    ald_markers <- relevant_fin_data %>%
      select(all_of(joining_id), has_asset_level_data, sectors_with_assets) %>%
      distinct()

    portfolio_subset <- left_join(portfolio_subset, ald_markers, by = joining_id)

    portfolio_subset <- portfolio_subset %>%
      rowwise() %>% m
      mutate(has_ald_in_fin_sector = if_else(grepl(financial_sector, sectors_with_assets),TRUE,FALSE))

    if(sum(portfolio_subset$value_usd, na.rm = T) != initial_port_value){stop("Merge over company id changes portfolio value")}

  }else{

    portfolio_subset <- portfolio_subset %>% add_column("has_asset_level_data","sectors_with_assets","has_ald_in_fin_sector")

  }
  return(portfolio_subset)

}

calculate_number_of_shares <- function(portfolio){

  portfolio <- portfolio %>%
    mutate(number_of_shares = ifelse(is.na(number_of_shares) & asset_type == "Equity", value_usd/unit_share_price, number_of_shares))

  return(portfolio)
}

create_id_columns <- function(portfolio, portfolio_type){

  if(portfolio_type == "Equity"){

    portfolio <- portfolio %>%
      rename(id = bloomberg_id) %>%
      mutate(id_name = "bloomberg_id",
             id = as.character(id))
  }
  if(portfolio_type == "Bonds"){

    portfolio <- portfolio %>%
      rename(id = corporate_bond_ticker) %>%
      mutate(id_name = "corporate_bond_ticker",
             id = as.character(id))
  }

  return(portfolio)

}

# FINAL SCRIPTS
get_and_clean_fin_data <- function(fund_data){

  # Financial Data
  fin_data_raw <- read_rds(paste0(analysis_inputs_path,"/security_financial_data.rda")) %>% as_tibble()
  # col_types = "ddcccccccccccccccDddddddddddddddddc")

  if(!unique(fin_data_raw$financial_timestamp) == financial_timestamp){print("Financial timestamp not equal")}

  overrides <- read_csv("data-raw/fin_sector_overrides.csv",
                        col_types = "ccdc")

  sector_bridge <- read_csv("data-raw/sector_bridge.csv", col_types = "cccccccc")

  fin_data <- fin_data_raw

  fin_data <- fin_data %>% filter(!is.na(isin))

  fin_data <- map_security_sectors(fin_data, sector_bridge)


  # Adds in the manual sector classification overrides
  fin_data <- override_sector_classification(fin_data, overrides)

  # Checks that only eq, cb, funds and others are in the fin_data
  fin_data <- check_asset_types(fin_data)

  # Checks for other mapped sectors not within the sector lists
  fin_data <- check_fin_mapped_sectors(fin_data)

  # TODO: find alternative here, calling in data from company financial data
  # Cleans and normalises the mapped_to_assets flag
  # fin_data <- check_mapped_assets_flag(fin_data)

  # Limits the Bonds category to corporate bonds only
  fin_data <- convert_corporate_bonds(fin_data)

  # Checks whether the bond is sovereign or not
  fin_data <- identify_sb(fin_data)

  # Checks to ensure all finds are classified as such
  fin_data <- classify_all_funds(fin_data)

  # fin_data <- add_bics_sector(fin_data)

  # Select relevant columns
  fin_data <- fin_data %>%
    select(
      company_id, company_name,bloomberg_id,corporate_bond_ticker,
      country_of_domicile,
      isin,
      unit_share_price, exchange_rate_usd,
      asset_type, security_type,
      security_mapped_sector, security_icb_subsector, security_bics_subgroup, # bclass4,
      maturity_date, coupon_value, amount_issued, current_shares_outstanding_all_classes, unit_share_price,
      sector_override,sector_boe,subsector_boe,sector_dnb,sector_ipr,subsector_ipr,
      is_sb
    )

  ### TEST
  if (nrow(fin_data) > nrow(fin_data_raw)){stop("Additional rows added to fin data")}

  # updates csv file with missing bloomberg data re funds
  if(data_check(fund_data))  check_funds_wo_bbg(fund_data,fin_data)

  return(fin_data)

}

add_bics_sector <- function(fin_data){

  bics_bridge <- read_csv("data-raw/bics_bridge.csv")

  fin_data_ <- left_join(fin_data, bics_bridge, by = c("security_bics_subgroup" = "bics_subsector"))


}

clean_unmatched_holdings <- function(portfolio){

  port_na <- portfolio %>% filter(is.na(security_mapped_sector))

  portfolio <- portfolio %>% filter(!is.na(security_mapped_sector))

  if(data_check(port_na)){

    port_na <- port_na %>%
      mutate(asset_type = "Unclassifiable",
             security_mapped_sector = "Unclassifiable",
             sector_boe = "Unclassifiable",
             sector_dnb = "Unclassifiable",
             sector_ipr = "Unclassifiable")
    portfolio <- rbind(portfolio, port_na)

  }

  return(portfolio)

}
