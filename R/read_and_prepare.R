read_and_prepare <- function(start_year, end_year) {

  data_location <- get_st_data_path()

  capacity_factors_power <- read_capacity_factors(
    path = file.path(data_location, "capacity_factors_WEO_2020.csv"),
    version = "new"
  )

  transition_scenarios <- read_transition_scenarios(
    path = file.path(data_location, "transition_scenario_input.csv"),
    start_of_analysis = start_year,
    end_of_analysis = end_year
  )

  return(list(capacity_factors_power = capacity_factors_power,
              transition_scenarios = transition_scenarios))

}
