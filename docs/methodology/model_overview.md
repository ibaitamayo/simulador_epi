# EPIDEM Model Overview

## Purpose

EPIDEM is a population-level epidemiological simulation platform designed for education, research, scenario exploration, and health-system planning exercises.

The model represents transmission and disease progression using aggregated compartmental dynamics rather than individual-level simulation.

---

## Core modelling approach

The platform currently supports:

- SIRD models
- SEIRD models

Compartments include:

- Susceptible (S)
- Exposed (E)
- Infectious (I)
- Recovered (R)
- Deceased (D)

The exposed compartment can be activated to represent a delay between infection and infectiousness.

---

## Geographic scope

The simulation operates at country level.

Each country is represented as an aggregated population unit connected through international mobility flows.

The model does not simulate individuals, households, workplaces, or specific contact networks.

---

## Mobility module

International spread is represented through aggregated passenger mobility.

Mobility flows are represented through a bilateral passenger matrix connecting participating countries.

Imported transmission pressure is generated according to:

- mobility volume;
- destination transmission intensity;
- remaining infectious pressure;
- scenario-specific modifiers.

---

## Demographic structure

The model can operate with age-adjusted population structures.

Country-specific age distributions are derived from external demographic sources.

Age groups are used to adjust:

- susceptibility assumptions;
- severity assumptions;
- mortality assumptions.

The objective is to improve realism while maintaining computational efficiency.

---

## Transmission dynamics

Transmission is controlled through:

- baseline reproduction number;
- dynamic effective reproduction number;
- containment measures;
- mobility effects;
- demographic structure.

The model is intended for comparative scenario analysis rather than forecasting.

---

## Containment measures

Containment measures can modify:

- transmission intensity;
- international mobility;
- timing of spread.

Measures are applied through predefined schedules or user-defined intervention scenarios.

---

## Comparator framework

EPIDEM includes support for fixed reference simulations.

The current implementation includes a COVID-19 Omicron reference comparator stored in external RDS files.

Comparators allow users to evaluate alternative scenarios against a stable baseline.

---

## Scenario laboratory

The scenario laboratory allows:

- scenario creation;
- scenario storage;
- scenario comparison;
- import/export;
- interpretation support.

The objective is to facilitate structured scenario exploration.

---

## Outputs

Current outputs include:

- epidemic curves;
- active infections;
- recovered population;
- cumulative deaths;
- geographic spread;
- country-level indicators;
- scenario comparisons;
- demographic summaries.

---

## Computational design

The platform is implemented in:

- R
- Shiny

The architecture combines:

- simulation engine;
- visualization layer;
- scenario management layer;
- regression testing framework.

---

## Intended use

EPIDEM is intended for:

- education;
- scientific research;
- public health training;
- scenario exploration;
- planning exercises.

The software is not intended for clinical decision-making or operational public health response without independent validation.

See DISCLAIMER.md for additional information.

