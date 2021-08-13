
### Portfolio cleaning functions
read_raw_portfolio_file <- function(project_name){

  portfolio <- NA

  input_path <- paste0(project_location, "/20_Raw_Inputs/")

  csv_to_read = list.files(path = input_path, pattern=paste0(project_name,"_Input.csv"))
  txt_to_read = list.files(path = input_path, pattern=paste0(project_name,"_Input.txt"))


  if (length(csv_to_read) == 1){portfolio <- read_csv(paste0(input_path,csv_to_read))}
  if (length(txt_to_read) == 1){
    enc <- guess_encoding(paste0(input_path,txt_to_read))$encoding[1]
    portfolio <- read.table(paste0(input_path,txt_to_read),sep = ",", header = T, fileEncoding = enc)
  }

  # Reads in Files saved with a ; not a ,
  if (ncol(portfolio) == 1 & length(csv_to_read) == 1){ portfolio <- read.csv(paste0(input_path,csv_to_read), strip.white = T, stringsAsFactors = F, sep = ";")}
  if (ncol(portfolio) == 1 & length(txt_to_read) == 1){ portfolio <- read.table(paste0(input_path,txt_to_read),sep = "\t", header = T, fileEncoding = enc)}
  if (ncol(portfolio) == 1 & length(txt_to_read) == 1){ portfolio <- read.table(paste0(input_path,txt_to_read),sep = ";", header = T, fileEncoding = enc)}


  if(!data_check(portfolio)){
    stop("No portfolio Input File")
  }

  portfolio
}

clean_colnames_portfolio_input_file <- function(portfolio){

  if (is.data.frame(portfolio)){
    # Removes additional columns added by Excel on saving
    portfolio <- portfolio[,!grepl("X",colnames(portfolio))]
  }else{
    stop("No portfolio Data readable")
  }

  portfolio <- janitor::clean_names(portfolio)

  if("numberof_shares" %in% colnames(portfolio)){portfolio<- portfolio %>% rename(number_of_shares = numberof_shares)}



  # names(portfolio)[1] <- gsub("[^A-Za-z0-9]", "", names(portfolio)[1])


  portfolio
}

clean_portfolio_col_types <- function(portfolio, grouping_variables){

  portfolio[,grouping_variables] <- lapply(portfolio[,grouping_variables], clean_punctuation)

  # portfolio$investor_name <- clean_punctuation(portfolio$investor_name)
  # portfolio$portfolio_name <- clean_punctuation(portfolio$portfolio_name)


  # portfolio$number_of_shares <- as.numeric(portfolio$number_of_shares)
  portfolio$market_value <- as.numeric(portfolio$market_value)
  portfolio$currency <- as.character(portfolio$currency)

  portfolio$currency <- if_else(portfolio$currency == "Euro","EUR",portfolio$currency)

  portfolio
}

clear_portfolio_input_blanks <- function(portfolio){

  if(any(portfolio[,grouping_variables] == ""| is.na(portfolio[,grouping_variables]))){
    print("Warning: missing grouping variables, corresponding rows removed")

    portfolio <- portfolio %>% filter_at(
      grouping_variables, all_vars(!is.na(.))
    )
  }

  portfolio

}

add_meta_portfolio <- function(portfolio, inc_meta_portfolio){

  portfolio_meta <- portfolio

  # Group at a level

  # lvl <- 1
  #
  #
  # gv <- grouping_variables[1:lvl]
  # ngv <- grouping_variables[seq(lvl+1,length(grouping_variables))]
  #
  # portfolio_1 <- portfolio_meta %>% mutate(!!!rlang::syms(ngv) = "Meta")
  #
  #
  # # loop through grouping variables
  # # the order of gv defines this
  #
  # for (g in length(grouping_variables)){
  #
  #   gv <- grouping_variables[g]
  #
  #   # portfolio_meta_sub <- [,gv]
  #
  #   portfolio_0 <- portfolio %>%
  #     mutate(country_group = "meta",
  #            investor_name = "meta",
  #            portfolio_name = "meta")
  #
  #   portfolio_1 <- portfolio %>%
  #     group_by(country_group) %>%
  #     mutate(investor_name = paste(country_group, "meta"),
  #     portfolio_name = paste(country_group, "meta"))
  #
  #   portfolio_2 <- portfolio %>%
  #     group_by(country_group, investor_name) %>%
  #     mutate(portfolio_name = paste(investor_name, "meta"))
  #
  #   portfolio_all <- bind_rows(portfolio,portfolio_0, portfolio_1, portfolio_2)
  #
  # }

  if(inc_meta_portfolio){
    # portfolio_meta$portfolio_name <- portfolio_meta$investor_name
    # portfolio <- rbind(portfolio,portfolio_meta)
    # # }
    #
    # if (inc_proj_metaportfolio){
    portfolio_meta$portfolio_name <- meta_portfolio_name
    portfolio_meta$investor_name <- meta_investor_name
    portfolio <- rbind(portfolio,portfolio_meta)
  }

  portfolio

}

add_holding_id <- function(portfolio){
  if (length(setdiff("holding_id", names(portfolio))) != 0) {
    portfolio$holding_id <- row.names(portfolio)
  }

  portfolio

}

check_missing_cols <- function(portfolio, grouping_variables){

  required_input_cols <- c("holding_id", "market_value","currency","isin",grouping_variables, "number_of_shares")

  if(!"number_of_shares" %in% colnames(portfolio)){portfolio$number_of_shares <- NA}


  missing_columns <-setdiff(required_input_cols,colnames(portfolio))

  if(length(missing_columns) > 0){
    stop(paste0("The input file is missing the following data columns: ", missing_columns))
  }

  portfolio <- as_tibble(portfolio)

  portfolio

}

set_currency_timestamp <- function(currencies){

  currencies <- currencies %>% mutate(ExchangeRate_2019Q2 = 1)

  currencies <- currencies %>%
    select(Currency_abbr, paste0("ExchangeRate_",financial_timestamp)) %>%
    filter(!is.na(Currency_abbr), Currency_abbr != "") %>% distinct()

  names(currencies)  <- c("currency", "exchange_rate")

  currencies$exchange_rate <- as.numeric(currencies$exchange_rate)

  currencies
}

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

map_comp_sectors <- function(comp_fin_data, sector_bridge){

  initial_no_rows = nrow(comp_fin_data)

  comp_fin_data <- comp_fin_data %>% left_join(sector_bridge %>% filter(source == "BICS") %>% select(-source),
                                               by = c("bics_subgroup" = "industry_classification"))

  comp_fin_data_na <- comp_fin_data %>% filter(is.na(sector)) %>% select(-sector)

  comp_fin_data <- comp_fin_data %>% filter(!is.na(sector))

  comp_fin_data_na <- comp_fin_data_na %>% left_join(sector_bridge %>% filter(source == "ICB")%>% select(-source),
                                                     by = c("icb_subgroup" = "industry_classification"))

  comp_fin_data <- comp_fin_data %>% bind_rows(comp_fin_data_na)

  comp_fin_data <- comp_fin_data %>% select(-mapped_sector) %>% rename(mapped_sector = sector)

  comp_fin_data %>% group_by(mapped_sector) %>% filter(is.na(mapped_sector)) %>% summarise(count = n(), .groups = "drop_last")

  comp_fin_data_na <- comp_fin_data %>% filter(is.na(mapped_sector))

  if(nrow(comp_fin_data) != initial_no_rows){stop("Rows being dropped in mapping sectors")}

  return(comp_fin_data)

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

normalise_fund_data <- function(fund_data){

  if(data_check(fund_data)){
    fund_data <- fund_data %>% group_by(fund_isin) %>%
      mutate(total_weight = sum(isin_weight,na.rm = T))

    fund_data_large <- fund_data %>% group_by(fund_isin) %>%
      filter(total_weight > 1) %>%
      mutate(isin_weight = isin_weight/total_weight) %>%
      select(-total_weight)

    fund_data_small <- fund_data %>% group_by(fund_isin) %>%
      filter(total_weight <= 1) %>%
      select(-total_weight)

    fund_data_missing <- fund_data_small %>%
      summarise(isin_weight = 1 - sum(isin_weight,na.rm = T),
                .groups = "drop_last") %>%
      mutate(holding_isin = "MissingValue")


    fund_data <- bind_rows(fund_data_large,fund_data_small,fund_data_missing)

    fund_data

  }else{
    stop("No fund data")
  }

  fund_data

}

### Portfolio Check Functions

convert_currencies <- function(portfolio, currencies){

  portfolio <- left_join(portfolio, currencies, by = "currency")

  portfolio$value_usd <- portfolio$market_value * portfolio$exchange_rate

  portfolio

}

add_fin_data <- function(portfolio, fin_data){

  portfolio_no_isin <- portfolio %>% filter(is.na(isin))

  portfolio_isin <- portfolio %>% filter(!is.na(isin))

  portfolio_fin <- left_join(portfolio_isin, fin_data, by = "isin")

  portfolio_fin <- bind_rows(portfolio_fin, portfolio_no_isin)

  portfolio_fin

}

calculate_value_usd_with_fin_data <- function(portfolio){

  # check correct inputs
  necessary_columns <- c("currency","unit_share_price")

  ### TEST
  if(! any( necessary_columns %in% colnames(portfolio))){stop("Portfolio not structured correctly")}


  # add missing currency for number of shares
  portfolio <- portfolio %>%
    mutate(currency = if_else(!is.na(number_of_shares), "USD", currency))

  # calculates the value_usd where number of shares are given
  portfolio <- portfolio %>%
    mutate(value_usd = if_else(
      asset_type %in% c("Equity", "Funds") & is.na(value_usd),
      number_of_shares * unit_share_price,
      value_usd
    ))

  portfolio
}

identify_fund_portfolio <- function(portfolio){

  fund_portfolio <- portfolio %>% filter(asset_type == "Funds", !is.na(isin))

  fund_portfolio

}

calculate_fund_portfolio <- function(fund_portfolio, fund_data, cols_portfolio_no_bbg = cols_portfolio, cols_funds = cols_of_funds){

  if (data_check(fund_portfolio)){
    fund_portfolio <-left_join(fund_portfolio, fund_data, by=c("isin"="fund_isin"), all.x = T)
    fund_portfolio$direct_holding <- FALSE

    fund_portfolio$original_value_usd <- fund_portfolio$value_usd
    fund_portfolio$value_usd <- fund_portfolio$isin_weight * fund_portfolio$value_usd
    fund_portfolio$fund_isin <- fund_portfolio$isin
    fund_portfolio$isin <- fund_portfolio$holding_isin

    # If there is no fund breakdown available, return the "original isin data" to the original locations
    fund_portfolio <- fund_portfolio %>%
      mutate(value_usd = if_else(!fund_isin %in% fund_data$fund_isin, original_value_usd, value_usd),
             isin = if_else(!fund_isin %in% fund_data$fund_isin, fund_isin, isin),
             direct_holding = if_else(!fund_isin %in% fund_data$fund_isin, TRUE, direct_holding),
      )

  }else{

    fund_portfolio <- fund_portfolio %>% bind_cols(data.frame(direct_holding = integer(0), fund_isin = character(0), original_value_usd = numeric(0)))

  }

  fund_portfolio <- fund_portfolio %>%  select(all_of(cols_portfolio_no_bbg), all_of(cols_funds))

  fund_portfolio


}

add_fund_portfolio <- function(portfolio, fund_portfolio, cols_of_funds){

  # Remove the fund lines from the portfolio
  portfolio_no_funds <- portfolio %>% filter(!isin %in% fund_portfolio$fund_isin)

  # Check that there are the correct number of isins in both portfolios
  if(nrow(portfolio_no_funds) + length(unique(fund_portfolio$holding_id)) != nrow(portfolio)){stop("Something unexpected with fund portfolio merge")}

  # Add additional fund relevant lines to original portfolio
  portfolio_no_funds <- portfolio_no_funds %>%
    mutate(direct_holding = TRUE,
           fund_isin = NA,
           original_value_usd = value_usd)

  # select same columns for both portfolios
  portfolio_no_funds <- portfolio_no_funds %>% select(colnames(portfolio), all_of(cols_of_funds))
  fund_portfolio <- fund_portfolio %>% select(colnames(portfolio), all_of(cols_of_funds))

  if(!identical(colnames(portfolio_no_funds), colnames(fund_portfolio))){stop("Colnames not equal, funds vs no funds")}

  # Merge in the results

  portfolio_total <- rbind(portfolio_no_funds, fund_portfolio)

  portfolio_total <- as_tibble(portfolio_total)

  portfolio_total


}

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

# Add Columns for missing or incorrect information
check_isin_format <- function(portfolio_total){

  portfolio_total <- portfolio_total %>%
    mutate(has_valid_isin = case_when(
      nchar(isin) != 12 ~ FALSE,
      isin == "" ~ FALSE,
      is.na(isin) ~ FALSE,
      grepl("[^[:alnum:]]", isin) ~ FALSE,
      TRUE ~ TRUE)
    )

  portfolio_total

}

check_missing_currency <- function(portfolio_total){

  # Currency blank or not in our currency data frame
  portfolio_total <- portfolio_total %>%
    mutate(has_currency = case_when(
      is.na(currency) ~ FALSE,
      currency == "" ~ FALSE,
      !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE)
    )

  portfolio_total
}

check_valid_input_value <- function(portfolio_total){

  # Currency negative or missing market value/number of shares
  portfolio_total <- portfolio_total %>%
    mutate(has_valid_input = case_when(
      is.na(market_value) & is.na(number_of_shares) ~ FALSE,
      market_value < 0 ~ FALSE,
      number_of_shares < 0 ~ FALSE,
      # !currency %in% currencies$currency ~ FALSE,
      TRUE ~ TRUE)
    )

  portfolio_total

}

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
get_and_clean_currency_data <- function(){

  currencies <- readRDS("data-raw/currencies.rda")

  currencies <- set_currency_timestamp(currencies)

  currencies

}

get_and_clean_fund_data <- function(){

  fund_data <- NA
  # Fund Data
  if(file.exists(paste0(analysis_inputs_path,"/fund_data_",financial_timestamp,".rda"))){
    fund_data <- readRDS(paste0(analysis_inputs_path,"/fund_data_",financial_timestamp,".rda"))
  }else if(file.exists(paste0(analysis_inputs_path,"/fund_data_2018Q4.rda"))){
    fund_data <- readRDS(paste0(analysis_inputs_path,"/fund_data_2018Q4.rda"))
    print("Old Fund Data being used. Replace FundsData2018Q4 or check name of file.")
  }else if(file.exists(paste0(analysis_inputs_path, "/SFC_26052020_funds.csv"))){
    fund_data <- read_csv(paste0(analysis_inputs_path, "/SFC_26052020_funds.csv"))
    print("2020Q2 SFC fund data being used")
  }else{
    if(!data_check(fund_data)){
      warning("No fund data available")}
  }


  if (data_check(fund_data)){

    fund_data <- fund_data %>% janitor::clean_names()

    fund_data <- fund_data %>% filter(!is.na(holding_isin) & holding_isin != "")

    fund_data <- normalise_fund_data(fund_data)

  }
  return(fund_data)
}

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

get_and_clean_company_fin_data <- function(){

  comp_fin_data_raw <- read_rds(paste0(analysis_inputs_path,"/consolidated_financial_data.rda")) %>%
    mutate(financial_sector=mapped_sector)

  comp_fin_data_raw <- comp_fin_data_raw %>% select(
    company_id, company_name, bloomberg_id, country_of_domicile, corporate_bond_ticker, bics_sector, bics_subgroup,
    icb_subgroup, mapped_sector,financial_sector, has_asset_level_data, has_assets_in_matched_sector, sectors_with_assets,
    current_shares_outstanding_all_classes, company_status, bond_debt_out,has_asset_level_data,
    financial_timestamp
  )

  sector_bridge <- read_csv("data-raw/sector_bridge.csv", col_types = "cccccccc") %>% select(industry_classification,source,sector)

  comp_fin_data <- map_comp_sectors(comp_fin_data_raw, sector_bridge)

  return(comp_fin_data)

}

get_and_clean_debt_fin_data <- function(){

  debt_fin_data_raw <- read_rds(paste0(analysis_inputs_path, "/debt_financial_data.rda"))

  return(debt_fin_data_raw)

}

process_raw_portfolio <- function(portfolio_raw,
                                  fin_data,
                                  fund_data,
                                  currencies,
                                  grouping_variables){

  portfolio <- clean_colnames_portfolio_input_file(portfolio_raw)

  portfolio <- clear_portfolio_input_blanks(portfolio)

  portfolio <- add_meta_portfolio(portfolio, inc_meta_portfolio)

  start_port_rows = nrow(portfolio)

  portfolio <- add_holding_id(portfolio)

  portfolio <- check_missing_cols(portfolio, grouping_variables)

  portfolio <- clean_portfolio_col_types(portfolio, grouping_variables)

  portfolio <- convert_currencies(portfolio, currencies)

  cols_portfolio <- colnames(portfolio)

  cols_of_funds <- c("direct_holding", "fund_isin","original_value_usd")

  # Add financial data
  # Merges in the clean data and calculates the marketvalue and number of shares
  portfolio <- add_fin_data(portfolio, fin_data)

  if(nrow(portfolio) != start_port_rows){stop("Portfolio lines changing unexpectedly")}

  portfolio <- calculate_value_usd_with_fin_data(portfolio)

  portfolio <- calculate_number_of_shares(portfolio)

  original_value_usd = sum(portfolio$value_usd, na.rm = T)

  # identify funds in the portfolio
  fund_portfolio <- identify_fund_portfolio(portfolio)

  if(data_check(fund_data)){
    # Creates the fund_portfolio to match the original portfolio
    fund_portfolio <- calculate_fund_portfolio(fund_portfolio, fund_data, cols_portfolio, cols_of_funds)

    # Merges in the bbg data to the fund portfolio
    fund_portfolio <- add_fin_data(fund_portfolio, fin_data)

    # add fund_portfolio and check that the total value is the same
    portfolio_total <- add_fund_portfolio(portfolio, fund_portfolio, cols_of_funds)
  } else{

    portfolio_total <- as_tibble(portfolio)
    portfolio_total$direct_holding <- TRUE

  }

  portfolio_total <- clean_unmatched_holdings(portfolio_total)

  if(round(sum(portfolio_total$value_usd, na.rm = T),1) != round(original_value_usd,1)){stop("Fund Portfolio introducing errors in total value")}


  ### TODO
  # summarise fund results
  # identify missing funds and isins
  ###

  return(portfolio_total)

}

add_portfolio_flags <- function(portfolio){
  ### FLAGS/Exclusions

  portfolio <- check_isin_format(portfolio)
  portfolio <- check_missing_currency(portfolio)
  portfolio <- check_valid_input_value(portfolio)
  portfolio <- check_bloomberg_data(portfolio)

  portfolio <- add_flags(portfolio)
  portfolio <- overall_validity_flag(portfolio)

  return(portfolio)
}

portfolio_summary <- function(portfolio_total){

  overview_data <- portfolio_total %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), asset_type, financial_sector, valid_input) %>%
    mutate(valid_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), asset_type, valid_input) %>%
    mutate(asset_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), valid_input) %>%
    mutate(portfolio_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    select(!!!rlang::syms(grouping_variables), asset_type, financial_sector, valid_input,
           valid_value_usd, asset_value_usd, portfolio_value_usd) %>%
    distinct()




  return(overview_data)



}

create_merged_portfolio <- function(eq_portfolio, cb_portfolio){

  portfolio_ <- rbind(eq_portfolio, cb_portfolio)

  portfolio_ <- portfolio_ %>% filter(!is.na(investor_name))

  if(!data_check(portfolio_)){warning("No portfolio data")}

  return(portfolio_)

}

create_portfolio_subset <- function(portfolio, portfolio_type, relevant_fin_data){

  portfolio <- portfolio %>% mutate(financial_sector = security_mapped_sector)

  if(portfolio_type %in% unique(portfolio$asset_type)){

    portfolio_subset <- portfolio %>% filter(asset_type == portfolio_type)

    # portfolio_subset <- check_for_ald(portfolio_subset,
    #                                   portfolio_type,
    #                                   relevant_fin_data)

    portfolio_subset <- create_id_columns(portfolio_subset, portfolio_type)

    portfolio_subset <- portfolio_subset %>%
      select(all_of(grouping_variables), holding_id, value_usd, number_of_shares,
             company_id, company_name, id, id_name, country_of_domicile, unit_share_price, current_shares_outstanding_all_classes,
             financial_sector)

  }else{
    print(paste0("No ",portfolio_type," in portfolio"))

    portfolio_subset <- NA
  }



  return(portfolio_subset)

}

create_audit_chart <- function(audit_file, proc_input_path){

  ## GRAPH VALUES
  base_size <- 12
  base_family <- "Clear Sans"
  font_color <- "#3D3D3C"

  #flag.Numbers.Colors <- brewer.pal(n = 8, name = "RdBu")
  ### red - "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" - blue

  flag_numbers <- audit_file %>%
    group_by(flag) %>%
    summarise(n = n(),
              .groups = "drop_last") %>%
    mutate(perc_n = n/sum(n))

  flag_numbers_colors <- c("Included in analysis" = "#207ddb",
                           "Holding not in Bloomberg database" = "#AAA9AD",
                           "Missing currency information" = "#848689",
                           "Negative or missing input value"= "#5B676D",
                           "Invalid or missing ISIN" = "#2A3439")

  flag_categories <- names(flag_numbers_colors)

  flag_numbers$flag = factor(flag_numbers$flag,
                             levels = flag_categories)

  y_intercept <- sum(flag_numbers$perc_n[flag_numbers$flag != "Included in analysis"])

  nrows <- length(unique(flag_numbers$flag))

  nrow_Legend <- ifelse(nrows <= 2, 3.2,
                        ifelse(nrows == 3 | nrows == 4, 3.7,
                               ifelse(nrows == 5, 3.8,
                                      ifelse(nrows == 6, 4.0,
                                             ifelse(nrows == 7, 4, 4.6)))))

  ## Chart
  Chart_1 <- ggplot(flag_numbers, aes(x = "", y = perc_n, fill = flag)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = flag_numbers_colors) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0.2),
                       #limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_discrete(expand = c(0, 0, 0, 0.9)) +
    ylab("By count of ISINs")+
    theme_minimal() +
    theme(
      axis.text.x = element_text(family = base_family, size = base_size, colour = font_color),
      axis.title = element_text(family = base_family, size = base_size, colour = font_color, hjust = 0.18),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.position = "bottom",
      legend.text = element_text(family = base_family, size = base_size, colour = font_color,
                                 margin = margin(l = 5, r = 10, unit = "pt")),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(color = "NA", fill = "NA"),
      plot.margin = unit(c(1, 0.1, 0.1, 0.1), "cm")) +
    guides(fill = guide_legend(reverse = TRUE, ncol = 1)) +
    geom_hline(aes(yintercept = y_intercept), color = "#3D3D3C", size = 1, show.legend = FALSE) +
    geom_text(aes(label = "Not included\nin analysis\n\n", x = 1.5, y_intercept-0.02),
              color = "#2e2e2e", family = base_family, size = base_size/2.5, hjust = 1) +
    geom_text(aes(label = "Included\nin analysis\n\n", x = 1.5, y_intercept+0.02),
              color = "#207ddb", family = base_family, size = base_size/2.5, hjust = 0)


  ggsave(paste0(proc_input_path,"/AuditChart.png"),Chart_1,height = nrow_Legend, width = 8)


}

create_audit_file <- function(portfolio_total){

  # portfolio_total <- left_join(portfolio_total, comp_fin_data %>% select(company_id, sectors_with_assets, bics_sector), by = "company_id")
  #
  # portfolio_total <- portfolio_total %>% rowwise() %>%
  #   mutate(has_assets = ifelse(is.na(sectors_with_assets), TRUE, FALSE),
  #          has_assets_in_fin_sector = grepl(pattern = financial_sector, x = sectors_with_assets)
  #   )

  audit_file <- portfolio_total %>%
    select(all_of(grouping_variables), holding_id, isin, value_usd, company_name, security_bics_subgroup, asset_type,  has_revenue_data, valid_input,
           direct_holding, financial_sector, sectors_with_assets, has_ald_in_fin_sector,flag)

  if(has_revenue == FALSE){audit_file <- audit_file %>% select(-has_revenue_data)}

  return(audit_file)

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

### Emissions work

get_average_emission_data <- function(inc_emission_factors){

  average_sector_intensity <- data.frame()

  if(inc_emission_factors){

    average_sector_intensity <- readRDS(paste0(analysis_inputs_path,"average_sector_intensity.rda"))
  }
  return(average_sector_intensity)
}

get_company_emission_data <- function(inc_emission_factors){

  company_emissions <- data.frame()

  if(inc_emission_factors){

    company_emissions <- readRDS(paste0(analysis_inputs_path,"company_emissions.rda"))
  }
  return(company_emissions)
}

prepare_portfolio_emissions <- function(
  audit_file,
  fin_data,
  comp_fin_data,
  average_sector_intensity,
  company_emissions
) {


  audit_file <- audit_file %>%
    janitor::clean_names(case = "snake")

  # prepare sector view
  company_bics_sector <- comp_fin_data %>%
    distinct(
      company_id,
      bics_sector,
      bics_subgroup,
      mapped_sector
    ) %>%
    filter(!is.na(company_id))

  # create audit view
  audit_file_view <- audit_file %>%
    distinct(
      holding_id,
      portfolio_name,
      investor_name,
      isin,
      value_usd
    )

  # connect audit to company ids
  audit_file_view <- fin_data %>%
    distinct(
      isin,
      company_id,
      company_name,
      asset_type
    ) %>%
    inner_join(
      audit_file_view,
      by = "isin"
    )

  # connect to consolidated financial data
  audit_file_view <- comp_fin_data %>%
    distinct(
      company_id,
      market_cap,
      bond_debt_out
    ) %>%
    inner_join(
      audit_file_view,
      by = "company_id"
    )

  # first try connecting at the company level
  audit_company_emissions <- audit_file_view %>%
    inner_join(
      company_emissions,
      # consolidated_company_emissions,
      by = c(
        "company_id",
        "company_name"
      )
    )

  # specify source
  audit_company_emissions <- audit_company_emissions %>%
    mutate(estimation_source = "Company data")

  # fix sectors
  # audit_company_emissions <- audit_company_emissions %>%
  #   mutate(ald_sector = ifelse(ald_sector %in% c("Cement", "Steel"), "Cement&Steel", ald_sector))
  #
  # save output
  # save(
  #   audit_company_emissions,
  #   file = path(here(), "EF", "Output", "sample_audit_company_emissions", ext = "rda")
  # )

  # create clean view
  audit_company_emissions <- audit_company_emissions %>%
    distinct(
      investor_name,
      portfolio_name,
      holding_id,
      isin,
      value_usd,
      company_id,
      company_name,
      asset_type,
      ald_sector,
      bics_sector,
      bics_subgroup,
      mapped_sector,
      emissions,
      estimation_source
    )

  # then try connecting at the sector level
  audit_file_sector <- audit_file_view %>%
    anti_join(
      audit_company_emissions,
      by = "company_id"
    )

  audit_file_sector <- audit_file_sector %>%
    inner_join(
      company_bics_sector,
      by = "company_id"
    )

  audit_sector_emissions <- audit_file_sector %>%
    inner_join(
      average_sector_intensity,
      by = c(
        "bics_sector",
        "asset_type"
      )
    )

  # calculate absolute emissions
  audit_sector_emissions <- audit_sector_emissions %>%
    mutate(
      emissions = case_when(
        asset_type == "Bonds" ~ bond_debt_out * mean_intensity,
        asset_type == "Equity" ~ market_cap * mean_intensity
      )
    )

  # flagging issues
  audit_sector_emissions <- audit_sector_emissions %>%
    mutate(
      issue = case_when(
        is.na(bond_debt_out) & asset_type == "Bonds" ~ "Missing debt outstanding",
        is.na(market_cap)  & asset_type == "Equity" ~ "Missing market capitilization"
      )
    )

  # specify source
  audit_sector_emissions <- audit_sector_emissions %>%
    mutate(estimation_source = "Sector average")

  # create clean view
  audit_sector_emissions <- audit_sector_emissions %>%
    distinct(
      investor_name,
      portfolio_name,
      holding_id,
      isin,
      value_usd,
      company_id,
      company_name,
      asset_type,
      bics_sector,
      bics_subgroup,
      mapped_sector,
      emissions,
      estimation_source,
      issue
    )

  # joining both sources together
  bind_rows(
    audit_sector_emissions,
    audit_company_emissions
  )
}

calculate_portfolio_emissions <- function(
  inc_emission_factors,
  audit_file,
  fin_data,
  comp_fin_data,
  average_sector_intensity,
  company_emissions
) {

  audit_sector_emissions <- data.frame()

  if(inc_emission_factors){

    audit_emissions <- prepare_portfolio_emissions(
      audit_file,
      fin_data,
      comp_fin_data,
      average_sector_intensity,
      company_emissions
    )

    # calculate holding weight
    audit_emissions <- audit_emissions %>%
      group_by(
        portfolio_name,
        investor_name
      ) %>%
      mutate(weighting = value_usd / sum(value_usd, na.rm = TRUE))

    # weight emissions by holding weight
    audit_emissions <- audit_emissions %>%
      mutate(weighted_emissions = weighting * emissions)


    audit_emissions <- add_other_to_sector_classifications(audit_emissions)

    # sum weighted emissions
    audit_sector_emissions <- audit_emissions %>%
      group_by(
        portfolio_name,
        investor_name,
        asset_type,
        sector
      ) %>%
      summarise(
        value_usd = sum(value_usd, na.rm = TRUE),
        weighted_sector_emissions = sum(weighted_emissions, na.rm = TRUE),
        .groups = "drop_last"
      )
  }

  audit_sector_emissions
}

add_other_to_sector_classifications <- function(audit){
  # fix sector classifications
  audit <- audit %>%
    mutate(ald_sector = ifelse(mapped_sector != "Other" & is.na(ald_sector), mapped_sector, ald_sector))

  # create final sector grouping
  audit <- audit %>%
    mutate(sector = ifelse(!is.na(ald_sector), ald_sector, bics_sector))

  # modify sector names
  audit <- audit %>%
    mutate(sector = ifelse(sector %in% c("Industrials", "Energy", "Utilities", "Materials"), paste0("Other ", sector), sector))

  audit
}
