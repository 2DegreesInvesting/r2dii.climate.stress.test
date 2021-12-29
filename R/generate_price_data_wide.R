#' Generates a toy data set of price data (wide) for all relevant technologies
#'
#' @description
#' Does not take any inputs. Generates a data frame that contains toy price
#' data in the format generally expected in the climate transition risk stress
#' test. This is done for all technologies.
#' @family example case functions
#' @return data frame
#' @export
generate_price_data_wide <- function() {
  test_price_data_wide <- tibble::tibble(
    year = rep.int(2020:2040, 12),
    sector = c(
      rep.int("Automotive", 63),
      rep.int("Coal", 21),
      rep.int("Oil&Gas", 42),
      rep.int("Power", 126)
    ),
    technology = c(
      rep.int("Electric", 21),
      rep.int("Hybrid", 21),
      rep.int("ICE", 21),
      rep.int("Coal", 21),
      rep.int("Gas", 21),
      rep.int("Oil", 21),
      rep.int("CoalCap", 21),
      rep.int("GasCap", 21),
      rep.int("HydroCap", 21),
      rep.int("NuclearCap", 21),
      rep.int("OilCap", 21),
      rep.int("RenewablesCap", 21)
    ),
    sector_unit_ds = c(
      rep.int("#", 63),
      rep.int("tonnes", 21),
      rep.int("GJ per day", 42),
      rep.int("MW", 126)
    ),
    price_unit_iea = c(
      rep.int("unit price", 63),
      rep.int("US$2010/tonne", 21),
      rep.int("US$2010/GJ", 42),
      rep.int("Dollars per MwH", 126)
    ),
    price_unit_etr = c(
      rep.int("unit price", 63),
      rep.int("USD per tonne", 21),
      rep.int("USD per MBTU", 21),
      rep.int("Dollars per barrel", 21),
      rep.int("USD per MWh", 126)
    ),
    B2DS = NA_real_,
    b2ds_source = "custom",
    NPS = c(
      rep.int(1, 63),
      rep.int(1, 21),
      rep.int(5, 21),
      seq(10, 20, by = 0.5),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075)
    ),
    nps_source = "custom",
    SDS = c(
      rep.int(1, 63),
      rep.int(1, 21),
      seq(5, 10, by = 0.25),
      rep.int(10, 21),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075)
    ),
    sds_source = "custom",
    Baseline = c(
      rep.int(1, 63),
      rep.int(1, 21),
      rep.int(5, 21),
      seq(10, 20, by = 0.5),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075),
      seq(0.03, 0.015, by = -0.00075)
    ),
    baseline_source = "custom"
  )
}
