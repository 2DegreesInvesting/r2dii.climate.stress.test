#' Generates a toy scenario data set for the technologies of the stylized examples
#'
#' @description
#' Does not take any inputs. Generates a data frame that contains toy scenario
#' data in the format generally expected in the climate transition risk stress
#' test. This is done for all technologies used in the stylized example cases.
#' @family example case functions
#' @return data frame
#' @export
generate_test_scenarios <- function() {
  test_scenario <- tibble::tibble(
    scenario_source = .env$scenario_source_test,
    scenario_geography = "Global",
    scenario = rep.int(c(rep.int("WEO2019_NPS", 21), rep.int("WEO2019_SDS", 21)), 8),
    ald_sector = c(
      rep.int("Oil&Gas", 84),
      rep.int("Power", 252)
    ),
    units = c(
      rep.int("cm", 42),
      rep.int("b/a", 42),
      rep.int("MW", 252)
    ),
    technology = c(
      rep.int("Gas", 42),
      rep.int("Oil", 42),
      rep.int("CoalCap", 42),
      # TODO: GasCap is marked as "declining" although generally the direction seems to be upward
      rep.int("GasCap", 42),
      rep.int("HydroCap", 42),
      rep.int("NuclearCap", 42),
      rep.int("OilCap", 42),
      rep.int("RenewablesCap", 42)
    ),
    year = rep.int(2020:2040, 16),
    direction = c(
      rep.int("declining", 42),
      rep.int("declining", 42),
      rep.int("declining", 42),
      # TODO: GasCap is marked as "declining" although generally the direction seems to be upward
      rep.int("declining", 42),
      rep.int("increasing", 42),
      rep.int("increasing", 42),
      rep.int("declining", 42),
      rep.int("increasing", 42)
    ),
    fair_share_perc = c(
      seq(0, -0.1, by = -0.005), seq(0, -0.2, by = -0.01),
      seq(0, -0.2, by = -0.01), seq(0, -0.3, by = -0.015),
      seq(0, -0.2, by = -0.01), seq(0, -0.3, by = -0.015),
      # TODO: GasCap actually "declining" in line with common tag. Correct?
      seq(0, -0.05, by = -0.0025), seq(0, -0.15, by = -0.0075),
      seq(0, 0.05, by = 0.0025), seq(0, 0.1, by = 0.005),
      seq(0, 0.05, by = 0.0025), seq(0, 0.15, by = 0.0075),
      seq(0, -0.2, by = -0.01), seq(0, -0.3, by = -0.015),
      seq(0, 0.15, by = 0.0075), seq(0, 0.3, by = 0.015)
    )
  )
}
