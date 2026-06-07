# Transmission Examples Framework

## Purpose

Transmission Examples provide literature-informed starting configurations for educational, exploratory and comparative analyses within the epidemiological simulation platform.

Transmission Examples are not intended to reproduce specific historical outbreaks, individual studies, or real-world events. They provide representative parameter sets that help users explore alternative transmission dynamics under a common modelling framework.

All parameters remain user-editable after loading an example.

---

## What This Simulator Is Not

This simulator is an educational and analytical population-level modelling tool.

It is designed to explore how different assumptions regarding transmission dynamics, population structure, mobility, interventions and uncertainty may influence aggregate outcomes under alternative scenarios.

The simulator should not be interpreted as:

- A forecasting system.
- A real-time surveillance platform.
- A decision-support system for operational outbreak management.
- A reproduction of any specific historical outbreak.
- A representation of individual-level transmission events.
- A representation of households, workplaces or social networks.
- A healthcare facility simulation.
- A laboratory model.
- A biological model of pathogen evolution.
- A clinical model of disease progression in individual patients.

The simulator operates exclusively at an aggregated population level.

Countries are represented as population units connected through simplified mobility mechanisms. Transmission dynamics are represented through aggregate epidemiological parameters rather than explicit biological or behavioural processes.

Results should be interpreted as scenario-dependent outputs generated under the assumptions selected by the user.

Differences between scenarios should generally be interpreted as comparative insights rather than as predictions of future events.

---

## Interpretation of Transmission Examples

Transmission Examples are not disease models.

A Transmission Example is a predefined set of literature-informed starting assumptions intended to illustrate a characteristic transmission pattern within the simulator.

Examples are provided because they are familiar reference points for users and educators.

Selecting an example does not imply that the simulator is reproducing the biology, ecology, clinical manifestations, healthcare pathways, or historical evolution of that disease.

Instead, examples should be understood as representative parameter configurations that help users explore how different combinations of transmission intensity, progression speed, severity, mobility and uncertainty influence aggregate population-level outcomes.

All examples are simplifications and should be interpreted within the Scope, Assumptions and Limitations documented for each example.

---

## Comparator versus Example

The platform distinguishes between two separate concepts.

### Reference Comparator

A Reference Comparator is a fixed, version-controlled scenario used for comparison across simulations.

Characteristics:

- Fixed parameters.
- Fixed implementation.
- Reproducible across versions.
- Not modified by user examples.

Current comparator:

- COVID-19 Omicron Reference Scenario.

### Transmission Example

A Transmission Example is a configurable starting configuration informed by published literature.

Characteristics:

- Loads recommended initial values.
- Fully editable by users.
- May evolve over time as evidence is updated.
- Intended for teaching, exploration and scenario comparison.

---

## Internal Architecture

Transmission Example

↓

Transmission Family

↓

Default Parameter Set

↓

Behaviour Settings

↓

References

↓

Scope, Assumptions and Limitations

---

## Initial Example Set

- Generic
- COVID-19
- Influenza
- Measles
- Ebola
- Cholera
- Tuberculosis

---

## Transmission Families

Transmission Families are internal implementation concepts.

They should not be the primary user-facing selector.

Initial families:

- Respiratory
- Regional Contact
- Water-Associated
- Slow Progression
- Generic

Additional families may be added in future versions.

---

## Evidence Requirements

Each Transmission Example must include:

- References.
- Evidence Level.
- Confidence Level.
- Scope.
- Assumptions.
- Limitations.

### Evidence Level

- High
- Moderate
- Exploratory

### Confidence Level

- High
- Moderate
- Low

Evidence Level and Confidence Level should be displayed separately.

---

## User Experience Principles

The system should remain simple for first-time users.

Default interface:

Transmission Example

- Generic
- COVID-19
- Influenza
- Measles
- Ebola
- Cholera
- Tuberculosis

Advanced information should be accessible through an optional **About this Example** panel containing:

- Description
- References
- Evidence Level
- Confidence Level
- Scope
- Assumptions
- Limitations

---

## Future Extensions

Potential future examples:

- SARS
- MERS
- Mpox
- Marburg
- 1918 Influenza
- Black Death

These additions should follow the same framework and documentation requirements.

---

## Versioning and Reproducibility

Transmission Examples may evolve over time as evidence becomes available and the platform develops.

Updates may affect:

- default parameter values;
- references;
- assumptions;
- limitations;
- confidence assessments.

Changes should be documented and version-controlled.

Simulation results obtained under different application versions may therefore differ even when using the same Transmission Example.

Reference Comparators should remain fixed whenever possible to preserve reproducibility across software versions.

---

## Evidence Governance

Transmission Examples should be reviewed periodically as new evidence becomes available.

Evidence updates should follow documented review procedures and be reflected in:

- references;
- evidence level;
- confidence level;
- assumptions;
- limitations;
- default parameter values.

The objective of updates is to improve educational value and scientific consistency rather than to reproduce specific real-world events.

Reference Comparators should only be modified through explicit versioning and documentation.
