# load packages
library(readxl)
library(dplyr)
library(tidyr)

# load flight data
# flight_data <- read_excel("../data/RStudio Air Travel Data.xlsx")

# anonymize & remove missing values
# flight_data <- flight_data %>%
#   select(-c(name, description)) %>%
#   drop_na(kilometers,cost)


# add data to package
# devtools::use_data(flight_data, overwrite = TRUE)

# EEIO Factor Package: The extented environmental input output factor is a proxy for estimating carbon emissions per
# dollar spent (kg CO2_e / $)

# calculate grams of CO2 assuming 175 grams per kilometer,
df <- flight_data %>%
  mutate(
    CO2_grams = kilometers * 175,
    CO2_kilograms = CO2_grams / 1000,
    CO2_tons = CO2_kilograms / 1000
    )

# summarize and calculate EEIO factor
df <- df %>%
  summarise(
    kilometers = sum(kilometers),
    spend_dollars = sum(cost),
    CO2_kilograms = sum(CO2_kilograms)
  ) %>%
  mutate(EEIO_factor = CO2_kilograms/spend_dollars)
