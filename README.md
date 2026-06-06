# EPIDEM

EPIDEM is an open-source R/Shiny application for population-level epidemiological simulation, teaching, scenario exploration, and health-system planning exercises.

The tool models aggregated transmission dynamics using compartmental structures such as SIRD and SEIRD, together with international mobility, demographic age structure, containment measures, and scenario comparison.

## Main features

- Population-level SIRD/SEIRD simulation.
- Exposed phase parameter for SEIRD scenarios.
- International spread using aggregated passenger mobility.
- Country-level demographic adjustment.
- Containment measure scenarios.
- Global curves for active cases, recovered population, and cumulative deaths.
- Country-level spread visualization.
- Scenario laboratory with save, restore, compare, export, and import options.
- COVID Omicron reference comparator loaded from RDS.
- Assumptions, limitations, and deployment checklist.
- Regression harness for refactoring safety.

## Intended use

EPIDEM is intended for teaching, epidemiology training, public health education, health-system planning exercises, scenario exploration, scientific communication, and methodological development.

It is not intended for clinical decision-making or operational public health response without independent validation.

See DISCLAIMER.md.

## Current app

The current working English app is:

app_epidemiologic_v17_academic_freeze.R

Development is performed on the English version. Translation to Spanish should be done only at the final stage, translating visible UI literals only.

## Repository structure

- app_epidemiologic_v17_academic_freeze.R
- R/
- tests/regression/
- docs/architecture/
- snapshots/
- DISCLAIMER.md
- LICENSE
- README.md

## Required files

The app expects the following files to be available in the project directory or configured data paths:

- country_age_distribution_wpp2024_6groups.rds
- country_age_distribution_wpp2024_6groups.csv
- world_countries_simplified.rds
- fixed_covid_omicron_reference_age_adjusted_seird.rds

Optional or legacy reference files may include:

- fixed_covid_omicron_reference_sir.rds
- fixed_covid_omicron_reference_age_adjusted.rds

## How to run

Open the project in RStudio and run:

shiny::runApp("app_epidemiologic_v17_academic_freeze.R")

Alternatively, from terminal:

Rscript -e 'shiny::runApp("app_epidemiologic_v17_academic_freeze.R")'

## Main dependencies

The app uses R/Shiny and common data visualization and spatial packages:

- shiny
- shinydashboard
- dplyr
- tidyr
- ggplot2
- plotly
- leaflet
- sf
- DT
- jsonlite

Install missing packages as needed:

install.packages(c("shiny", "shinydashboard", "dplyr", "tidyr", "ggplot2", "plotly", "leaflet", "sf", "DT", "jsonlite"))

## Regression tests

Run:

Rscript tests/regression/run_regression_suite.R

Expected result:

PASS / INFO

## Development rules

- Development is performed on the English version.
- Do not translate internal object names, functions, IDs, column names, or programmatic identifiers.
- Translate only visible UI strings at the final stage.
- Do not mix refactoring and new features in the same change.
- Keep simulation logic separate from UI code where possible.
- Run the regression suite after every structural change.

## License

This project is distributed under the MIT License.

See LICENSE.
