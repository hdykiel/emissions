#' Carbon Emissions Calculator
#'
#' Estimate carbon dioxide emissions from travel
#'
#' @param category category to calculate emissions for. Currently supported are: "airplane" for travel.
#' @param method The calculation method to use: 'distance' for distance-based , or 'spend' for spend-based.
#' @param value an integer value representing kilometers traveled for distance-based method, or US dollars spent for spend-based method.
#'
#' @return CO2 emissions in tons
#' @import dplyr
#' @import readr
#' @export
#'
#' @examples
#' emissions("airplane", "distance", 1000)
#'
emissions <- function(category = "airplane", method = "distance", value = 1) {

  co2_emissions <- suppressMessages(readr::read_csv(system.file("conf", "co2_emissions.conf", package = "emissions")))

  # check for valid arguments
  # this will need to get more sophisticated once we add new types of categories (non-travel)
  available_methods <- co2_emissions$Method
  if (!(method %in% available_methods)) stop("Please provide a valid method")

  available_categories <- co2_emissions$Category
  if (!(category %in% available_categories)) stop("Please provide a valid category")

  conversion_factor <- co2_emissions %>%
    dplyr::filter(Category == category, Method == method) %>%
    dplyr::select(CO2_kg) %>%
    dplyr::pull()

  CO2_kg <- conversion_factor * value
  CO2_ton <- CO2_kg / 1000
  CO2_ton

}

