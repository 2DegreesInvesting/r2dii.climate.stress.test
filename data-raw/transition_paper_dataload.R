
### CGFI PAPER SPECIAL ======

# abcd_input <- readr::read_csv("CGFI paper/project_input_ST_INPUTS_MASTER_adjabcd_adjCF 2/abcd_stress_test_input.csv")
#
# abcd_filtered <- all_crispy%>%
#   distinct(company_name, sector, business_unit)%>%
#   inner_join(abcd_input,
#              by=c("company_name",
#                   "sector"="ald_sector",
#                   "business_unit"="technology"))

# # compare identifier with raw
# abcd_input%>%rename(sector=ald_sector, business_unit=technology)%>% group_by(company_name,sector) %>% mutate(all_unique=length(unique(plan_tech_prod)) == 1) %>% distinct(company_name, sector, business_unit, all_unique) %>%   dplyr::inner_join(
#   readxl::read_excel("CGFI paper/Production_Identifierv2.xlsx") %>% select(c("company_name","business_unit", "onlyconstant")),
#   by = c("company_name", "business_unit")
# ) %>% arrange(company_name, sector)
#
# # compare identifier with crispy filtered
# abcd_filtered%>% group_by(company_name,sector) %>% mutate(all_unique=length(unique(plan_tech_prod)) == 1) %>% distinct(company_name, sector, business_unit, all_unique) %>%   dplyr::inner_join(
#   readxl::read_excel("CGFI paper/Production_Identifierv2.xlsx") %>% select(c("company_name","business_unit", "onlyconstant")),
#   by = c("company_name", "business_unit")
# ) %>% arrange(company_name, sector)

abcd_input <- readr::read_csv("CGFI paper/project_input_ST_INPUTS_MASTER_adjabcd_adjCF 2/abcd_stress_test_input.csv")
abcd_filtered <- abcd_input %>%
  rename(sector=ald_sector, business_unit=technology)

# # load identifier
# production_identifier <- readxl::read_excel("CGFI paper/Production_Identifierv2.xlsx")
# make identifier
production_identifier <- abcd_filtered%>%
  group_by(company_name,sector) %>%
  mutate(onlyconstant=length(unique(plan_tech_prod)) == 1) %>%
  distinct(company_name, sector, business_unit, onlyconstant) %>%
  ungroup()



all_crispy_no_constant_companies <- all_crispy %>%
  dplyr::inner_join(
    production_identifier,
    by = c("company_name","business_unit", "sector")
  ) %>%
  dplyr::filter(as.logical(onlyconstant) == F)

# all_crispy_no_constant_companies %>%
#   readr::write_csv("CGFI paper/multi_crispy_no_constant_companies.csv")

all_crispy_agg <- all_crispy_no_constant_companies %>%
  # filter(    term == 5
  #            # ,sector=="Power"
  # ) %>%
  group_by(company_name  ,
           sector,
           scenario_duo,
           baseline_scenario,
           shock_scenario,
           term) %>%
  summarise(
    net_present_value_baseline = sum(net_present_value_baseline),
    net_present_value_shock = sum(net_present_value_shock),
    pd_baseline = mean(pd_baseline),
    pd_shock=mean(pd_shock)
  )%>%
  mutate(
    net_present_value_difference = net_present_value_shock - net_present_value_baseline,
    net_present_value_rate_of_change = round(net_present_value_difference) / net_present_value_baseline,
    pd_difference = pd_shock - pd_baseline,
    pd_rate_of_change = pd_difference/pd_shock
  ) %>%
  ungroup()



# all_crispy_agg %>% filter(scenario_duo
#                           %in% c("NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
#                                  "WEO2021_STEPS&WEO2021_SDS"))
#
# power_rates_of_change <- all_crispy_agg %>% filter(term==5,
#                                shock_scenario %in% c("IPR2021_FPS", "IPR2021_RPS", "NGFS2021_REMIND_B2DS",
#                                                          "Oxford2021_fast", "WEO2021_SDS")
#                                ,sector=="Power") %>%
#   tidyr::pivot_wider(id_cols="company_name", values_from = "net_present_value_rate_of_change", names_from = "scenario_duo")
# tout <- inner_join(power_rates_of_change, ref%>%select(-c("...1")))
# toutcor <- cor(tout[,2:ncol(too)], use="everything")
# # data.frame(toutcor)[names(ref %>% select(-c("...1", company_name))),
# #         names(power_rates_of_change %>% select(-c(company_name)))]
#


#
# power_rates_of_change <- all_crispy_agg %>%
#   filter(
#     scenario_duo %in% c("NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS")
#   ) %>%
#   tidyr::pivot_wider(id_cols = "company_name",
#                      values_from = "net_present_value_rate_of_change",
#                      names_from = "scenario_duo")
# # tout <- inner_join(power_rates_of_change, ref%>%select(-c("...1")))
# # toutcor <- cor(tout[,2:ncol(tout)], use="everything")
# onlynew <- cor(power_rates_of_change[,2:ncol(power_rates_of_change)])
#
#
#
# ref <- readr::read_csv("CGFI paper/df_wide.csv")
#
#
#
# a <- all_crispy_agg %>%
#   filter(sector=="Power",
#          baseline_scenario=="NGFS2021_REMIND_CP",
#          shock_scenario=="NGFS2021_REMIND_B2DS",
#          term==5)
#
# b <- all_crispy_agg %>%
#   filter(sector=="Power",
#          baseline_scenario=="Oxford2021_base",
#          shock_scenario=="Oxford2021_fast",
#          term==5)
#
# diff_a <- dplyr::inner_join(a, ref, by="company_name") %>% select(company_name, net_present_value_rate_of_change, percent_change_NGFS2021_REMIND_B2DS)
# diff_b<- dplyr::inner_join(b, ref, by="company_name") %>% select(company_name, net_present_value_rate_of_change, percent_change_Oxford2021_fast)


# all_crispy <-readr::read_csv("CGFI paper/multi_crispy_no_constant_companies.csv")
#
# names(all_crispy) <-c("company_name", "sector", "business_unit", "roll_up_type",
#                       "scenario_geography", "baseline_scenario", "shock_scenario",
#                       "lgd", "risk_free_rate", "discount_rate", "dividend_rate",
#                       "growth_rate", "shock_year", "net_present_value_baseline",
#                       "net_present_value_shock", "net_present_value_difference", "term",
#                       "pd_baseline", "pd_shock", "pd_difference", "scenario_duo", "run_id",
#                       "zero_prod", "const_prod")

# all_crispy <- all_crispy %>% rename(company_name_hash=company_name)%>%inner_join(mpr) %>% inner_join(df_wide%>%distinct(company_name))
nz_duos <-
  c(
    "IPR2021_baseline&IPR2021_RPS",
    "Oxford2021_base&Oxford2021_fast",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_NZ2050",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    # "WEO2021_APS&WEO2021_NZE_2050",
    "WEO2021_STEPS&WEO2021_NZE_2050" # stated policy scenario == current policies ?
  )

dt_duos <-
  c(
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT"
  )

dn0_duos <-
  c(
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0"
  )

b2ds_duos <-
  c(
    "IPR2021_baseline&IPR2021_FPS",
    # "NGFS2021_REMIND_NDC&NGFS2021_REMIND_B2DS",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS",
    # "NGFS2021_MESSAGE_NDC&NGFS2021_MESSAGE_B2DS",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS",
    # "NGFS2021_GCAM_NDC&NGFS2021_GCAM_B2DS",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS",
    # "WEO2021_APS&WEO2021_SDS",
    "WEO2021_STEPS&WEO2021_SDS"
  )



all_crispy_target_named <- all_crispy_agg%>%
  dplyr::mutate(
    target_duo = dplyr::case_when(
      scenario_duo %in% nz_duos ~ "NZ2050",
      scenario_duo %in% dt_duos ~ "DT",
      scenario_duo %in% dn0_duos ~ "DN0",
      scenario_duo %in% b2ds_duos ~ "B2DS",
      .default = "other"
    )
  )

remind_duos <-
  c(
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_NZ2050",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DT",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_DN0",
    "NGFS2021_REMIND_CP&NGFS2021_REMIND_B2DS"
  )

message_duos <-
  c(
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_NZ2050",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DT",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_DN0",
    "NGFS2021_MESSAGE_CP&NGFS2021_MESSAGE_B2DS"
  )

gcam_duos <-
  c(
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_NZ2050",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DN0",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_DT",
    "NGFS2021_GCAM_CP&NGFS2021_GCAM_B2DS"
  )

iea_duos <- c(# stated policy scenario == current policies ?
  "WEO2021_STEPS&WEO2021_NZE_2050",
  "WEO2021_STEPS&WEO2021_SDS")

ipr_duos <- c("IPR2021_baseline&IPR2021_RPS",
              "IPR2021_baseline&IPR2021_FPS")


oxford_duos <-
  c("Oxford2021_base&Oxford2021_fast")


all_crispy_scenario_named <- all_crispy_target_named %>%
  dplyr::mutate(
    scenario_provider = dplyr::case_when(
      scenario_duo %in% remind_duos ~ "REMIND",
      scenario_duo %in% message_duos ~ "MESSAGE",
      scenario_duo %in% gcam_duos ~ "GCAM",
      scenario_duo %in% iea_duos ~ "IEA",
      scenario_duo %in% ipr_duos ~ "IPR",
      scenario_duo %in% oxford_duos ~ "OXFORD",
      .default = NA
    )
  )


use_duos <-
  c(remind_duos,
    message_duos,
    gcam_duos,
    iea_duos,
    ipr_duos,
    oxford_duos)



all_crispy_filtered <- all_crispy_scenario_named %>%
  dplyr::inner_join(
    readr::read_csv("CGFI paper/df_wide.csv") %>% distinct(company_name)
  )%>%
  dplyr::filter(scenario_duo %in% use_duos,
                term == 5
                ,sector=="Coal")

# %>%
#   select(-c(sector)) %>%
#   rename(sector=business_unit)


all_crispy_filtered <- all_crispy_filtered %>% mutate(
  scenario_duo_bckp=scenario_duo,
  scenario_duo=purrr::map_chr(stringr::str_split(scenario_duo, "&"), function(x) {x[2]})
)
