#' Generates a toy data set of capacity factors for the power technologies
#'
#' @description
#' Does not take any inputs. Generates a data frame that contains toy capacity
#' factors data in the format generally expected in the climate transition risk
#' stress test. This is done for all technologies in the Power sector and the
#' scenarios used in the stylized examples.
#' @family example case functions
#' @return data frame
#' @export
generate_test_capacity_factors <- function() {
  test_capacity_factors <- tibble::tibble(
    scenario = c(rep.int("NPS", 126), rep.int("SDS", 126)),
    scenario_geography = "Global",
    technology = rep.int(
      c(
        rep.int("CoalCap", 21),
        rep.int("GasCap", 21),
        rep.int("HydroCap", 21),
        rep.int("NuclearCap", 21),
        rep.int("OilCap", 21),
        rep.int("RenewablesCap", 21)
      ), 2
    ),
    year = rep.int(2020:2040, 12),
    capacity_factor = c(
      rep.int(0.5, 21),
      rep.int(0.5, 21),
      rep.int(0.4, 21),
      rep.int(0.8, 21),
      rep.int(0.3, 21),
      rep.int(0.3, 21),
      seq(0.5, 0.2, by = -0.015),
      seq(0.5, 0.3, by = -0.01),
      rep.int(0.4, 21),
      rep.int(0.8, 21),
      seq(0.3, 0.2, by = -0.005),
      rep.int(0.3, 21)
    )
  )
}
