# styler: off
stress_test_arguments <- tibble::tribble(
                      ~name,       ~type, ~default,               ~allowed,       ~min,       ~max,
               "asset_type", "character",       NA, "equity, bonds, loans",         NA,         NA,
        "baseline_scenario", "character", "WEO2020_SPS", "WEO2019_SPS, WEO2020_SPS, GECO2019_ref",         NA,         NA,
           "shock_scenario", "character", "WEO2020_SDS", "WEO2019_SDS, WEO2020_SDS, GECO2019_1.5c, GECO2019_2c_m",         NA,         NA,
                      "lgd",    "double",   "0.45",                     NA,      "0.3",      "0.9",
           "risk_free_rate",    "double",   "0.02",                     NA,        "0",     "0.05",
            "discount_rate",    "double",   "0.07",                     NA,    "0.015",      "0.1",
              "growth_rate",    "double",   "0.03",                     NA,     "0.01",    "0.099",
  "div_netprofit_prop_coef",    "double",      "1",                     NA,      "0.8",        "1",
               "shock_year",    "double",   "2030",                     NA,     "2025",     "2035",
            "fallback_term",    "double",      "2",                     NA,        "1",        "5", # NOTE: before changing check cap_terms().
       "scenario_geography", "character", "Global", "AdvancedEconomies, Africa, AsiaPacific, Brazil, CentralandSouthAmerica, China, DevelopingEconomies, Emergingmarket&developingeconomies, EU, Eurasia, Europe, Global, India, Japan, LatinAmerica, MiddleEast, NonOECD, NorthAmerica, OECD, Russia, SouthAfrica, SoutheastAsia, US",         NA,         NA,
        "use_company_terms",   "logical",  "FALSE",          "TRUE, FALSE",         NA,         NA,
               "start_year",    "double",   "2021",                     NA,     "2021",     "2021"
)
# styler: on

usethis::use_data(stress_test_arguments, overwrite = TRUE)
