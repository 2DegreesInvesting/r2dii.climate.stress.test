#' Generates a toy data set of price data (long) for all relevant technologies
#'
#' @description
#' Does not take any inputs. Generates a data frame that contains toy price
#' data in the format generally expected in the climate transition risk stress
#' test. This is done for all technologies used in the stylized examples.
#' @family example case functions
#' @return data frame
#' @export
generate_price_data <- function() {
  test_price_data <- tibble::tibble(
    year = rep.int(2020:2040, 16),
    source = "WEO2019",
    scenario = c(rep.int("SPS", 168), rep.int("SDS", 168)),
    scenario_geography = "Global",
    technology = rep.int(
      c(
        rep.int("Gas", 21),
        rep.int("Oil", 21),
        rep.int("CoalCap", 21),
        rep.int("GasCap", 21),
        rep.int("HydroCap", 21),
        rep.int("NuclearCap", 21),
        rep.int("OilCap", 21),
        rep.int("RenewablesCap", 21)
      ), 2
    ),
    indicator = rep.int(
      c(
        rep.int("price", 42),
        rep.int("LCOE", 126)
      ), 2
    ),
    unit = rep.int(
      c(
        rep.int("usd/Mbtu", 21),
        rep.int("usd/tonne", 21),
        rep.int("$/MWh", 126)
      ), 2
    ),
    price = c(
      seq(7.5, 8.5, by = 0.05),
      seq(70, 100, by = 1.5),
      seq(300, 350, by = 2.5),
      seq(300, 350, by = 2.5),
      seq(400, 200, by = -10),
      seq(350, 300, by = -2.5),
      seq(300, 350, by = 2.5),
      seq(400, 200, by = -10),
      rep.int(7.5, 21),
      seq(70, 60, by = -0.5),
      seq(300, 450, by = 7.5),
      seq(300, 400, by = 5),
      seq(400, 200, by = -10),
      seq(350, 300, by = -2.5),
      seq(300, 400, by = 5),
      seq(400, 200, by = -10)
    )
  )
}
