## Scope

`emissions` calculates greenhouse gas (GHG) emissions (as CO2 equivalents, *CO2e*) from business-related activities. Its scope is continually being extended. Currently, we are covering:

-   emissions resulting from business travel (scope 3)

-   emissions resulting from electricity use (scope 2)

## Context

The [GHG Protocol](ghgprotocol.org) distinguishes three scopes of GHG emissions:

-   Scope 1 emissions result from operations occurring at the reporting organization's site (direct emissions).

-   Scope 2 emissions are a subset of indirect emissions, namely, those originating from purchased electricity, heat or steam.

-   Scope 3 emissions include all remaining indirect ones, e.g., business travel, waste disposal, etc.

For GHG reporting, two types of information are generally needed, so-called *activity data* and *emission factors*. Activity data vary by category, while emission factors relate amounts of activity to amounts of GHG emission.

## Data requirements

### Business travel and employee commuting (scope 3, category 6)

For business travel, the most frequently-used type of activity data is travel distance (as opposed to fuel consumption, which usually will not be available.) Emission factors then indicate GHG emissions per distance traveled.

In some cases, distance data will not be available. As a workaround, spend (e.g., flight tickets) can be related to distance, which will then be used for GHG calculation. For these cases, GHG recommends that organizations determine appropriate mappings based on their own data. To avoid any pretension of (non-existent) accuracy, our package does not offer a spend-based way of calculating emissions from business travel.

#### Emission factors

Emission factors for business travel and employee commuting, including air travel as well as rail and road transport, are taken from the [U.S. Environmental Protection Agency](epa.gov)'s [2018 GHG emission factors inventory](https://www.epa.gov/sites/production/files/2018-03/documents/emission-factors_mar_2018_0.pdf), which indicates its reliance on the following primary sources:

`CO2, CH4, and N2O emissions data for highway vehicles are from Table 2-13 of the Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990–2015. Vehicle-miles and passenger-miles data for highway vehicles are from Table VM-1 of the Federal Highway Administration Highway Statistics 2015.`

`Fuel consumption data and passenger-miles data for rail are from Tables A.14 to A.16 and 9.10 to 9.12 of the Transportation Energy Data Book: Edition 35. Fuel consumption was converted to emissions by using fuel and electricity emission factors presented in the tables above.`

`Air Travel factors from 2017 Guidelines to Defra / DECC's GHG Conversion Factors for Company Reporting. Version 1.0 August 2017.`

##### Notes: Air travel

Our primary reason for not directly using the [conversion tables](https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2020) issued by the UK [Department for Business, Energy & Industrial Strategy](https://www.gov.uk/government/organisations/department-for-business-energy-and-industrial-strategy) (which seems to have replaced the above-cited *Defra* as issuer of those tables) is that these are more specifically tailored to the UK, while we expect most users of this package (at least initially) to come from the US.

##### Notes: Public transport (rail/road)

For rail transport, specify the exact category (intercity, commuter, or transit) whenever possible. If that information is not available, we will try to determine the type based on kilometers traveled, with cutoffs arbitrarily set to \< 10 kms for transit and \> 30 kms for intercity (with everything else in-between).

*Caution:* Emission factors for public transport depend on energy sources used, and thus may not be appropriate for non-US accounting.

### Electricity (scope 2)

To report emissions from electricity use, you need energy consumption data, aggregated on a yearly basis.

CO2 emissions differ by region, due to varying composition of energy sources employed. Regional "resolution" is extendable: If a region is missing, you are welcome to contribute the data!

Currently, we envisage a maximal resolution of `country + state`, with both being required for the US (e.g. "US, Massachusetts"), but state being optional for other countries.

##### Note on green house gases involved

While in the context of electricity, we see emission factors listed for `SO2` and `NOx`, these are not greenhouse gases. Cf. <https://www.eia.gov/tools/faqs/faq.php?id=76&t=7>:

`SO2 and NOx are criteria pollutants regulated by the U.S. Environmental Protection Agency (EPA) under the Clean Air Act and subsequent amendments. They are not greenhouse gases. The NOx emissions and emissions factors published by EIA do not include emissions of nitrous oxide (N2O), which is a greenhouse gas (GHG). The only GHG emissions estimates and GHG emissions factors that EIA publishes are energy-related CO2 emissions.`

Consequently, the CO2e value reported by `emissions` for electricity comprises CO2 only.

## Installation

You can install emissions from github with:

``` {.r}
# install.packages("devtools")
devtools::install_github("hdykiel/emissions")
```

## Examples

``` {.r}
# calculate emissions (tons of CO2e) for a 3 plane trips of 100, 1000, and 10000 kms each
emissions("airplane", c(100, 1000, 10000))

# emissions from electricity use (tons of CO2e), calculated separately for 2 years
# energy consumption in kWh, valid for Massachusetts, US
emissions("electricity", list(list(2018, 2019), list(777, 888)), "US|MA")
```
