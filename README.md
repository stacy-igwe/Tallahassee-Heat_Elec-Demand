# Tallahassee-Heat_Elec-Demand

This repository stores code for the paper titled **"Extreme Heat on Residential Electricity Demand: A Case Study in a Hot-Humid Climate"** an analysis of heat index and residential electricity consumption in Tallahassee, FL.

This study examines the relationship between extreme heat (via heat index) and residential energy demand using daily household electricity consumption for residents of Tallahassee, Florida during the May-September 2019 period. Our analysis uses heat index in capturing the relationship between ambient temperature and humidity while controlling for other weather variables (e.g., precipitation and wind speed) to examine heterogeneity by income grouping for household electricity demand response. We further analyze a subset of households that participated in a rebate energy-efficiency program.

## Repository Overview

* Merging and cleaning residential electricity consumption data
* Constructing household and premises identifiers
* Restricting the analysis to the May–September 2019 study period
* Calculating relative humidity and heat index using temperature and dew-point data
* Merging electricity, weather, and census-derived demographic data
* Construction of income quintiles
* Estimating segmented relationships between heat index and electricity consumption
* Estimating household fixed-effects models
* Comparing heat-response relationships across income quintiles
* Analyzing households participating in energy-efficiency rebate programs
* Producing figures and regression tables used in the analysis

## Study Context

**Location:** Leon County, Florida (Tallahassee)
**Period:** May - September 2019
**Outcome:** Natural log of daily residential electricity consumption (kWh)
**Primary Weather Variable:** Heat Index (°F) as calculated by the Rothfusz Equation
**Heterogeneity examined:** Census tract median household income and participation in residential energy-efficiency rebate programs.

## Data

Available upon request.

