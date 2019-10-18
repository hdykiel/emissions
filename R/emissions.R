#' Carbon Emissions Calculator
#'
#' Estimate carbon dioxide emissions from travel
#'
#' @param transport_mode
#' @param kilometers
#'
#' @return
#' @export
#'
#' @examples
#' emissions("plane", 1000)
emissions <- function(transport_mode = "plane", kilometers = 1) {

  if (transport_mode == "plane") {
    CO2_grams <- 175*kilometers # C02 grams
    CO2_kg <- CO2_grams/1000
    CO2_ton <- CO2_kg/1000
    return(CO2_ton)

  } else {

    stop("please enter a valid transportation mode")

  }
}
