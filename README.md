# SNAP-Ed Community Networks Reshaping App

## Description

The SNAP-Ed Community Networks Dashboard was used by [Illinois SNAP-Ed](https://inep.extension.illinois.edu/who-we-are/our-story/snap-ed) to map SNAP-Ed community networks to census tracts. 
Illinois Extension defines a SNAP-Ed community network as a geographical area where SNAP-Ed eligible adults live, work, shop, and access community resources.
The app allows users to select census tracts for a community network and export their GOIDs as a CSV.

## Application

The SNAP-Ed Community Networks Reshaping app is accessible via [this link](https://jstadni2.shinyapps.io/Community_Networks_Reshaping/).

## Development

Several of the packages used to build this app were in development during intial deployment. Unfortunately, the dependency versions were not saved during deployment. As of 10-20-2022,
subsequent attempts to deploy the app were unsuccessful. The [SNAP-Ed Community Networks Dashboard](https://github.com/jstadni2/communitynetworksdashboard) and
[INEP Community Profile](https://github.com/jstadni2/inepdashboard) were both forked from this repo, and may provide useful reference for rebuiling the SNAP-Ed Community Networks Reshaping App
with the [renv](https://rstudio.github.io/renv/articles/renv.html) package for dependency management.

## Data Sources

### Shapefiles

Shapefiles used to create SNAP-Ed Community Networks were obtained from the sources below:

- [Census Bureau Cartographic Boundary Files](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html)
- [Chicago Community Areas](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6)

### Community Sites

The following sources are utilized for community sites:

- Family Community Resource Centers (FCRC) and Women, Infants, and Children (WIC) Offices - [IDHS Office Locator](https://www.dhs.state.il.us/page.aspx?module=12)
- Federally Qualified Health Centers (FQHC) - [HRSA Data by Geography](https://data.hrsa.gov/geo)

### SNAP-Ed Eligibility

SNAP-Ed Eligibility metrics (individuals/percentage of inviduals with income at or below 185% of the Federal Poverty Level) are provided by the 2019 
[American Community Survey](https://www.census.gov/programs-surveys/acs/about.html) 5-year estimates.

- Poverty Status: [S1701 POVERTY STATUS IN THE PAST 12 MONTHS](https://data.census.gov/cedsci/table?q=185%20federal%20poverty%20level&tid=ACSST5Y2020.S1701)

