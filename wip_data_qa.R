# run project initialisation from main script to load data_location and required dependencies

library(dplyr)
library(ggplot2)

# TODO: add proper initialisation



#---time series annual profits---------

# TODO: decide on good graphs to look at

# equity
qa_annual_profits_eq <- equity_annual_profits %>%
  dplyr::mutate(year_of_shock = transition_scenario$year_of_shock)

eq_annual_profits_over_time_ls <- qa_annual_profits_eq %>%
  ggplot(aes(x = year, y = net_profits_ls, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

eq_annual_profits_over_time_ls

eq_disc_annual_profits_over_time_ls <- qa_annual_profits_eq %>%
  ggplot(aes(x = year, y = discounted_net_profit_ls, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

eq_disc_annual_profits_over_time_ls

eq_production_changes_over_time <- qa_annual_profits_eq %>%
  ggplot(aes(x = year, y = late_sudden, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

eq_production_changes_over_time

# bonds
qa_annual_profits_cb <- bonds_annual_profits %>%
  dplyr::mutate(year_of_shock = transition_scenario$year_of_shock)

cb_annual_profits_over_time_ls <- qa_annual_profits_cb %>%
  ggplot(aes(x = year, y = net_profits_ls, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

cb_annual_profits_over_time_ls

cb_disc_annual_profits_over_time_ls <- qa_annual_profits_cb %>%
  ggplot(aes(x = year, y = discounted_net_profit_ls, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

cb_disc_annual_profits_over_time_ls

cb_production_changes_over_time <- qa_annual_profits_cb %>%
  ggplot(aes(x = year, y = late_sudden, color = scenario_name, group = scenario_name)) +
  geom_line() +
  facet_wrap(. ~ technology, scales = "free")

cb_production_changes_over_time


