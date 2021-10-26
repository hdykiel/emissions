#' Carbon Emissions Calculator
#'
#' Estimate green house gas emissions (CO2e)
#'
#' @param category category to calculate emissions for. Currently supported are:
#'  - "airplane" for air travel;
#'  - "intercity" for long-distance trains, "commuter" for suburban trains, or "transit"
#'   for urban rail services. If the exact category is unknown, specify "rail". In this case,
#'   we try to determine the actual type from the distance;
#'  - "car" as well as "bus", "motorcycle" and "light-duty truck" for road vehicles;
#'  - "electricity"
#'  - "natural gas"
#' @param value category-dependent value.
#' - For all forms of travel, integer vector of kilometers traveled. E.g., `value = c(220,800,2500)`.
#' - For electricity, a list of two lists, the first one containing the years to calculate
#'   emissions for, and the second, respective energy consumption in kilowatt-hours (kWh).
#'   E.g., `value = list(list(2018, 2019), list(7777, 8888))`
#' - For natural gas, an integer vector of Mcf consumed
#' @param additional_info category-dependent value, not required for every category.
#' - For travel: not required.
#' - For electricity: geographic location; a string containing country and state abbreviations,
#'   separated by |. For example: "US|MA" for Massachusetts. For the US, state is always required.
#'   For other countries, it may be left out (together with the separating |).
#'
#' @return CO2 equivalent emissions in tons
#' @import dplyr
#' @import readr
#' @export
#'
#' @examples
#' emissions("airplane", c(100, 1000, 10000))
#' emissions("rail", c(1, 10, 100))
#' emissions("electricity", list(list(2018, 2019), list(777, 888)), "US|MA")
#'
emissions <- function(category, value, additional_info = NULL) {

  rail_types <- c("rail", "intercity", "commuter", "transit")
  road_types <- c("road", "car", "bus", "motorcycle", "light-duty truck")
  available_categories <- c("airplane", rail_types, road_types, "electricity", "natural_gas")

  if (!(category %in% available_categories)) stop("Please provide a valid category")

  if (category == "electricity") {
    if (length(value) != 2) {
      stop("value has to be a list of two lists (years / kWh, resp.) E.g.: list(list(2018, 2019), list(777, 888))")
    }
    if (is.null(additional_info)) {
      stop(paste0("Please provide a region for electricity usage. Regions are concatenations of ",
                  "country|state, if available (required if country == US), or just country otherwise. ",
                  "E.g.: US|MA, France, Germany|Bavaria."))
    }
  }

  equivalents <- suppressMessages(readr::read_csv(system.file("conf", "global_warming_potential.conf", package = "emissions")))
  ch4_eq <- equivalents %>% dplyr::filter(ghg == "ch4") %>% dplyr::select(factor) %>% dplyr::pull()
  n2o_eq <- equivalents %>% dplyr::filter(ghg == "n2o") %>% dplyr::select(factor) %>% dplyr::pull()

  co2_eq <- if (category == "airplane") co2e_airplane(value, c(ch4_eq, n2o_eq))
    else if (category %in% rail_types) co2e_rail(value, category, c(ch4_eq, n2o_eq))
    else if (category %in% road_types) co2e_road(value, category, c(ch4_eq, n2o_eq))
    else if (category == "electricity") co2e_electricity(value, additional_info)
    else if (category == "natural_gas") co2e_natural_gas(value, additional_info)

  co2_ton <- co2_eq / 1000
  co2_ton
}

#' @import dplyr
co2e_road <- function(value, category, equivalents) {

  ch4_eq <- equivalents[1]
  n2o_eq <- equivalents[2]

  conf <- suppressMessages(readr::read_csv(system.file("conf", "road.conf", package = "emissions")))
  conf <- conf %>% mutate(
    co2_kg = km_to_mile(co2_kg),
    ch4_g = km_to_mile(ch4_g),
    n2o_g = km_to_mile(n2o_g)
  )

  distance_traveled <- sum(value)
  type <- conf %>% filter(type == category)
  distance_traveled * (
    type$co2_kg +
      type$ch4_g / 1000 / ch4_eq +
      type$n2o_g / 1000 / n2o_eq
  )
}

#' @import dplyr
co2e_rail <- function(value, category, equivalents) {

  ch4_eq <- equivalents[1]
  n2o_eq <- equivalents[2]

  conf <- suppressMessages(readr::read_csv(system.file("conf", "rail.conf", package = "emissions")))
  conf <- conf %>% mutate(
    co2_kg = km_to_mile(co2_kg),
    ch4_g = km_to_mile(ch4_g),
    n2o_g = km_to_mile(n2o_g)
  )

  co2_eq <- if (category != "rail") {
    distance_traveled <- sum(value)
    type <- conf %>% filter(type == category)
    distance_traveled * (
      type$co2_kg +
      type$ch4_g / 1000 / ch4_eq +
      type$n2o_g / 1000 / n2o_eq
    )
  } else {
    # if the exact type is not known, we try to infer it from distance
    intercity_threshold <- 30
    commuter_threshold <- 10

    transit_kms <- Filter(function(x) x < commuter_threshold, value) %>% sum()
    commuter_kms <- Filter(function(x) between(x, commuter_threshold, intercity_threshold), value) %>% sum()
    intercity_kms <- Filter(function(x) x > intercity_threshold, value) %>% sum()

    transit_factors <- conf %>%
      dplyr::filter(type == "transit") %>%
      dplyr::select(co2_kg, ch4_g, n2o_g)

    commuter_factors <- conf %>%
      dplyr::filter(type == "commuter") %>%
      dplyr::select(co2_kg, ch4_g, n2o_g)

    intercity_factors <- conf %>%
      dplyr::filter(type == "intercity") %>%
      dplyr::select(co2_kg, ch4_g, n2o_g)

    transit_co2eq <-
      transit_kms * (
        transit_factors$co2_kg +
          transit_factors$ch4_g / 1000 / ch4_eq +
          transit_factors$n2o_g / 1000 / n2o_eq
      )

    commuter_co2eq <-
      commuter_kms * (
        commuter_factors$co2_kg +
          commuter_factors$ch4_g / 1000 / ch4_eq +
          commuter_factors$n2o_g / 1000 / n2o_eq
      )

    intercity_co2eq <-
      intercity_kms * (
        intercity_factors$co2_kg +
          intercity_factors$ch4_g / 1000 / ch4_eq +
          intercity_factors$n2o_g / 1000 / n2o_eq
      )

   sum(c(transit_co2eq, commuter_co2eq, intercity_co2eq))
  }

  co2_eq

}


#' @import dplyr
co2e_airplane <- function(value, equivalents) {

  ch4_eq <- equivalents[1]
  n2o_eq <- equivalents[2]

  conf <- suppressMessages(readr::read_csv(system.file("conf", "air_travel.conf", package = "emissions")))

  conf <- conf %>% mutate(
    co2_kg = km_to_mile(co2_kg),
    ch4_g = km_to_mile(ch4_g),
    n2o_g = km_to_mile(n2o_g)
  )

  threshold_medium <- mile_to_km(300) # could also be parsed from conf$additional_comments
  threshold_long <- mile_to_km(2300)

  short_flight_factors <- conf %>%
    dplyr::filter(haul == "short") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  medium_flight_factors <- conf %>%
    dplyr::filter(haul == "medium") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  long_flight_factors <- conf %>%
    dplyr::filter(haul == "long") %>%
    dplyr::select(co2_kg, ch4_g, n2o_g)

  short_flight_kms <- Filter(function(x) x < threshold_medium, value) %>% sum()
  medium_flight_kms <- Filter(function(x) between(x, threshold_medium, threshold_long), value) %>% sum()
  long_flight_kms <- Filter(function(x) x > threshold_long, value) %>% sum()

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

  sum(c(short_flight_co2eq, medium_flight_co2eq, long_flight_co2eq))

}

#' @import dplyr
#' @import purrr
co2e_electricity <- function(value, additional_info) {

  split <- strsplit(additional_info, "[|]") %>% unlist()
  country_ <- split[1]
  state_ <- split[2]

  # 27 EU countries plus UK, Turkey, Island, Norway
  countries_eu <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom","Turkey","Island","Norway")
  eu_countries_with_state <- c("Germany")

  yearly_emissions <- if (country_ == "US") {

    if (is.na(state_)) {
      stop("Need state if country is US. E.g.: US|WA.")
    }

    conf <- suppressMessages(readr::read_csv(system.file("conf", "electricity_us.conf", package = "emissions")))

    validate_years(conf, value)

    purrr::map2(value[[1]], value[[2]], function(year_, kwh) {
      lbs_mwh <- conf %>% filter(country == country_, state == state_, year == year_) %>%
        select(co2_lbs_mwh) %>% pull()
      value_mwh <- kwh / 1000
      co2_kg <- lb_to_kg(value_mwh * lbs_mwh)
      co2_kg
    })

  } else if (country_ %in% countries_eu) {

    conf <- if (is.na(state_)) {

    ########################################## to generate #################################

    # eu <- readr::read_csv("~/Downloads/2017_CO2_IntensEL_EEA.csv")
    # df <- eu %>% filter(!stringr::str_detect(CountryShort, "^EU\\d")) %>%
    #   transmute(country = CountryLong, year = Year, co2_g_kwh = ValueNumeric) %>%
    #   tibble::add_column(state = NULL) %>%
    #   tibble::add_column(source = "https://www.eea.europa.eu/data-and-maps/data/co2-intensity-of-electricity-generation")
    # df %>% readr::write_csv("electricity_eu_global.conf")

    ########################################################################################

      suppressMessages(readr::read_csv(system.file("conf", "electricity_eu_global.conf", package = "emissions")))

      } else if (!is.na(state_) && country_ %in% eu_countries_with_state) {

        filename <- paste0("electricity", "_eu_", tolower(country_), ".conf")
        suppressMessages(readr::read_csv(system.file("conf", filename, package = "emissions")))

      }

    validate_years(conf, value)

    purrr::map2(value[[1]], value[[2]], function(year_, kwh) {
      g_kwh <- conf %>% filter(country == country_, state == if (!is.na(state_)) state_ else "unknown", year == year_) %>%
        select(co2_g_kwh) %>% pull()
      co2_kg <-kwh * g_kwh / 1000
      co2_kg
    })

  } else {

  stop(paste0("No emission factors available for region: ", additional_info,
              ". Consider opening a PR to add them."))
  }

  unlist(yearly_emissions)

}

#' @import dplyr

co2e_natural_gas <- function(value, additional_info){
  # write function here
}

# helper functions

validate_years <- function(conf, value) {

  min_year <- min(conf$year)
  max_year <- max(conf$year)
  years <- value[[1]] %>% unlist() %>% sort()
  if (min(years) < min_year || max(years) > max_year) stop(paste0("Not all years available. Available range is ",
                                                                  min_year, " - ", max_year, "."))
}

# conversions
mile_to_km <- function(miles) miles * 1.609344
km_to_mile <- function(km) km * 0.6213712
lb_to_kg <- function(lb) lb * 453.59237 / 1000


