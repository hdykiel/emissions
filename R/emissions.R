#' Carbon Emissions Calculator
#'
#' Estimate carbon dioxide emissions from travel
#'
#' @param transport_mode
#' @param method
#' @param value
#'
#' @return
#' @export
#'
#' @examples
#' emissions("plane", 1000)
emissions <- function(transport_mode = "plane", method = "distance", value = 1) {

  if (transport_mode == "plane" && method == "distance" ) {
    CO2_grams <- 175*value # C02 grams
    CO2_kg <- CO2_grams/1000
    CO2_ton <- CO2_kg/1000
    return(CO2_ton)

  } else if (transport_mode == "plane" && method == "spend") {
    CO2_kg <- 0.15*value
    CO2_ton <- CO2_kg/1000
    return(CO2_ton)

  } else {
    stop("please enter a valid transportation mode")
  }
}
