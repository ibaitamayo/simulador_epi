# Model Scope, Assumptions and Limitations

## Scope

EPIDEM is an aggregated population-level simulation platform for education, research, scenario exploration and planning exercises.

It represents populations, countries, mobility and transmission dynamics at a macroscopic level.

## What the model is not

EPIDEM is not:

- a forecasting system;
- a real-time surveillance platform;
- a clinical decision-support tool;
- a reproduction of a specific outbreak;
- an individual-level model;
- a household, workplace or contact-network model;
- a healthcare facility simulation;
- a laboratory model;
- a biological model of within-host processes.

## Core assumptions

The model assumes:

- countries are aggregated population units;
- transmission is represented through aggregate epidemiological parameters;
- mobility is represented through simplified connectivity mechanisms;
- age structure can modify severity and mortality assumptions;
- scenario outputs depend on user-selected parameters;
- comparisons between scenarios are more informative than isolated absolute outputs.

## Interpretation

Outputs should be interpreted as scenario-dependent results under explicit assumptions.

They should not be interpreted as predictions, forecasts, or direct estimates of future real-world events.

## Transmission Examples

Transmission Examples are literature-informed starting configurations.

They are not disease models and do not reproduce specific historical events.

All parameters remain editable after loading an example.

## Reference Comparator

The COVID-19 Omicron Reference Scenario is a fixed comparator.

It is separate from Transmission Examples and should not be overwritten by example profiles.

## Limitations

Important limitations include:

- simplified country-level mixing;
- no explicit contact networks;
- no explicit healthcare-system network;
- simplified mobility;
- no individual-level trajectories;
- no direct representation of clinical pathways;
- no direct representation of laboratory or biological mechanisms.

## Intended use

EPIDEM is intended for:

- teaching;
- scientific communication;
- methodological exploration;
- sensitivity analysis;
- scenario comparison;
- planning exercises.

Operational use requires independent validation.

---

## Versioning and Reproducibility

Simulation outputs depend on:

- application version;
- model structure;
- geographical coverage;
- mobility assumptions;
- calibration assumptions;
- user-selected parameters.

Transmission Examples provide starting configurations rather than fixed outcomes.

As the platform evolves, results generated using identical user inputs may differ between software versions.

Reference Comparators are intended to provide stable comparison points across versions whenever possible.

---

## Evidence Governance

Default parameter configurations should be supported by documented evidence whenever available.

Updates to evidence sources, assumptions or default values should be tracked through version control and reflected in the relevant documentation.

The platform prioritizes transparency, reproducibility and explicit documentation of modelling assumptions.
