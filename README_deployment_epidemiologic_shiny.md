# Epidemiological Shiny simulator — deployment checklist

## Required runtime files

Place these files in the same deployment directory as the Shiny app:

- `country_age_distribution_wpp2024_6groups.rds`
- `world_countries_simplified.rds`
- `fixed_covid_omicron_reference_age_adjusted_seird.rds`

Optional legacy/reference files:

- `fixed_covid_omicron_reference_sir.rds`
- `fixed_covid_omicron_reference_age_adjusted.rds`

## Runtime behaviour

The app does not generate or write the SEIRD comparator RDS at runtime. The comparator must be created outside the hosted app and bundled in the deployment environment.

## Main model defaults

- Guided mode uses SEIRD.
- Default exposed period: 4 days.
- Default active phase duration: 20 days.
- Default containment window in manual controls: start day 210, end day 240.
- Map animation interval: 100 ms.

## Scenario laboratory

The scenario laboratory stores scenarios in the Shiny session. For reuse across sessions, export the JSON configuration and import it later.
