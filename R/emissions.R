#' Carbon Emissions Calculator
#'
#' Estimate carbon dioxide emissions from travel
#'
#' @param transport_mode mode of transport: 'plane' for air travel
#' @param method The calculation method to use: 'distance' for distance-based , or 'spend' for spend-based.
#' @param value an integer value representing kilometers traveled for distance-based method, or US dollars spent for spend-based method.
#'
#' @return
#' @export
#'
#' @examples
#' emissions("plane", "distance", 1000)
#'
emissions <- function(transport_mode = "plane", method = "distance", value = 1) {

  if (transport_mode == "plane") {
    if (method == "distance") {
      CO2_grams <- 175 * value # C02 grams
      CO2_kg <- CO2_grams / 1000
      CO2_ton <- CO2_kg / 1000
      CO2_ton
    }
    else if (method == "spend") {
    CO2_kg <- 1.321209 * value # EEIO factor
    CO2_ton <- CO2_kg / 1000
    CO2_ton
    }
    else {
      stop("please enter a valid method")
    }
  }
  else {
    stop("please enter a valid transportation method")
  }
}

# eeio_factor(data = input data set)
