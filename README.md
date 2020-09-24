# emissions

The `emissions` package helps calculate greenhouse gas emissions from travel activities. The methodology used to estimate emissions is based on guidance provided by the Greenhouse Gas Protocol, an emissions accounting standard established to help companies report greenhouse gas emissions accurately.

## Installation

You can install emissions from github with:


``` r
# install.packages("devtools")
devtools::install_github("hdykiel/emissions")
```

## Examples

``` r
# calculate emissions (CO2 tons) for a 1,000 kilometer plane trip
emissions("plane", "distance", 1000)
```

If you have spend data from Expensify, you can do e.g.:

```
library(expensify)
library(dplyr)

expensify_data <- summarize_expensify(expenses, category = "airplane", year = "2020")
spend <- expensify_data %>% 
  filter(Classification == "Airplanes") %>%
  select(Spend) %>%
  pull()

spend %>% emissions(category = "airplane", method = "distance")
```



  
