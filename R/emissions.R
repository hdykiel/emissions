#' Carbon Emissions Calculator
#'
#' Estimate green house gas emissions from travel
#'
#' @param category category to calculate emissions for. Currently supported are:
#'  "airplane" for air travel.
#' @param type category-dependent additional information. For air travel: "distance" or "spend".
#' If you choose "spend", a default conversion factor will be used that maps spend to distance.
#' @param value category-dependent value. For air travel, integer vector of either kilometers
#' traveled or spend in USD. E.g., `value = c(220,800,2500)`.
#'
#' @return CO2 equivalent emissions in tons
#' @import dplyr
#' @import readr
#' @export
#'
#' @examples
#' emissions("airplane", "distance", c(100, 1000, 10000))
#'
emissions <- function(category, type, value) {

  available_categories <- c("airplane")
  if (!(category %in% available_categories)) stop("Please provide a valid category")

  co2e_ton <- switch(category,
         airplane = co2e_airplane(type, value)
  )
  co2e_ton
}

#' @import dplyr
co2e_airplane <- function(method, value) {

  equivalents <- suppressMessages(readr::read_csv(system.file("conf", "global_warming_potential.conf", package = "emissions")))
  ch4_eq <- equivalents %>% dplyr::filter(ghg == "ch4") %>% dplyr::pull()
  n2o_eq <- equivalents %>% dplyr::filter(ghg == "n2o") %>% dplyr::pull()


  conf <- suppressMessages(readr::read_csv(system.file("conf", "air_travel.conf", package = "emissions")))
  threshold_medium <- miles_to_km(300) # could also be parsed from conf$additional_comments
  threshold_long <- miles_to_km(2300)

  short_flight_factors <- conf %>%
    dplyr::filter(haul == "short") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  medium_flight_factors <- conf %>%
    dplyr::filter(haul == "medium") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  long_flight_factors <- conf %>%
    dplyr::filter(haul == "long") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  # aller-retour?
  # maybe have to use an "average" haul ...
  if (method == "spend") {
    kms <- Map(price_to_km_air_travel, value) %>% unlist()
  } else if (method == "distance") {
    kms <- value
  } else {
    stop("Valid methods for air travel are distance or spend")
  }

  short_flight_kms <- Filter(function(x) x < threshold_medium, kms) %>% sum()
  medium_flight_kms <- Filter(function(x) between(x, threshold_medium, threshold_long), kms) %>% sum()
  long_flight_kms <- Filter(function(x) x > threshold_long, kms) %>% sum()

  short_flight_co2eq <-
    short_flight_kms * (
      short_flight_factors$co2_kg +
        short_flight_factors$ch4_g / 1000 / ch4_eq +
        short_flight_factors$n2o_g / 1000 / n2o_eq
    )

  medium_flight_co2eq <-
    medium_flight_kms * (
      medium_flight_factors$co2_kg +
        medium_flight_factors$ch4_g / 1000 / ch4_eq +
        medium_flight_factors$n2o_g / 1000 / n2o_eq
    )

  long_flight_co2eq <-
    long_flight_kms * (
      long_flight_factors$co2_kg +
        long_flight_factors$ch4_g / 1000 / ch4_eq +
        long_flight_factors$n2o_g / 1000 / n2o_eq
    )

  co2_kg <- sum(c(short_flight_co2eq, medium_flight_co2eq, long_flight_co2eq))
  co2_ton <- co2_kg / 1000
  co2_ton

}

miles_to_km <- function(miles) miles * 1.609344

price_to_km_air_travel <- function(price) (price - 50)/0.11

