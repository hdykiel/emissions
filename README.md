# emissions

The `emissions` package helps calculate greenhouse gas emissions from travel activities. The methodology used to estimate emissions is based on guidance provided by the Greenhouse Gas Protocol, an emissions accounting standard established to help companies report greenhouse gas emissions accurately.

## Installation

You can install emissions from github with:


``` r
# install.packages("devtools")
devtools::install_github("hdykiel/emissions")
```

## Example

``` r
# calculate emissions (CO2 tons) for a 1,000 kilometer plane trip
emissions("plane", "distance", 1000)
```
