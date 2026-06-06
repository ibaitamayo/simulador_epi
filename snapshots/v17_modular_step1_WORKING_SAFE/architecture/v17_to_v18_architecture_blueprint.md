# Shiny epidem — v17 to v18 architecture blueprint

## Strategic decision

Continue development on the English R/Shiny version, but stop treating the app as a single monolithic file. The short-term target is a stable academic/mesomanagement v17. The medium-term target is a modular v18 where the simulation engine, UI modules, documentation, and visible UI text registry are separated.

## Non-negotiable rules

1. Work only on the English version during development.
2. Translate only at the final stage.
3. Translate only visible UI literals.
4. Never translate internal object names, function names, input/output IDs, column names, or programmatic identifiers.
5. Do not change the core SEIRD/SIRD engine until a regression baseline exists.
6. Do not mix the academic Scenario Lab with the future gamified EpiDecision Room.

## Current file audited

`app_epidemiologic_v16_scenario_lab_step6b_basic_exposed_parameter.R`

Approximate size: 6,632 lines.

Current high-level blocks:

- constants and country data;
- helper functions;
- age distribution and country polygons;
- containment functions;
- dynamic diagnostics;
- UI definition;
- server logic;
- scenario lab logic;
- reference comparator loading;
- Shiny app launch.

## Recommended project structure

```text
shiny-epidem/
│
├── app.R
│
├── R/
│   ├── constants.R
│   ├── country_data.R
│   ├── model_seird.R
│   ├── mobility.R
│   ├── age_adjustment.R
│   ├── containment.R
│   ├── scenario_lab_engine.R
│   ├── calibration.R
│   ├── situational_awareness.R
│   ├── summaries.R
│   ├── diagnostics.R
│   ├── reference_comparator.R
│   ├── ui_text_registry.R
│   └── utils.R
│
├── modules/
│   ├── mod_world_map.R
│   ├── mod_global_results.R
│   ├── mod_scenario_lab.R
│   ├── mod_variant_dynamics.R
│   ├── mod_country_spread.R
│   ├── mod_age_cost.R
│   ├── mod_calibration.R
│   ├── mod_containment.R
│   ├── mod_situational_awareness.R
│   ├── mod_simulator_logic.R
│   ├── mod_scope_assumptions.R
│   └── mod_r0_analysis.R
│
├── data/
│   ├── country_age_distribution_wpp2024_6groups.rds
│   ├── country_age_distribution_wpp2024_6groups.csv
│   ├── world_countries_simplified.rds
│   └── fixed_covid_omicron_reference_age_adjusted_seird.rds
│
├── docs/
│   ├── bibliography/
│   ├── assumptions/
│   ├── deployment/
│   └── validation/
│
├── www/
│   ├── css/
│   ├── icons/
│   └── infographics/
│
└── tests/
    ├── testthat/
    └── regression_snapshots/
```

## First safe extraction order

### Step 1 — Externalize static/helper code
Low-risk extraction. No UI behavior should change.

- `format_big()` → `R/utils.R`
- `scenario_multiplier()` → `R/utils.R`
- `variant_calibration_values()` → `R/utils.R`
- `haversine_km()` → `R/mobility.R`
- country constants → `R/country_data.R`
- age helper functions → `R/age_adjustment.R`
- containment helper functions → `R/containment.R`

### Step 2 — Create visible UI string registry
Create a central registry of visible literals. Do not replace all strings immediately; first identify and review them.

Initial seed file created separately as:

`ui_visible_strings_seed.csv`

### Step 3 — Regression baseline before engine edits
Before changing the simulation engine, capture baseline outputs for 3–5 standard scenarios:

1. Spain, default, SEIRD, no containment.
2. Spain, default, SEIRD, early moderate containment.
3. United States, high mobility, no containment.
4. Random country fixed seed, dynamic module off.
5. Dynamic reference preset, fixed seed.

Minimum baseline metrics:

- final active;
- peak active;
- day of peak;
- final deaths;
- final recovered;
- countries reached;
- top 10 affected countries;
- age summary;
- scenario config JSON.

### Step 4 — Modularize UI tabs
After regression baseline, split UI/server by tab. Start with tabs that do not alter simulation state:

1. Scope, assumptions & limitations.
2. Simulator logic.
3. R0 Analysis.
4. Age adjustment & cost.
5. Containment measures.
6. Calibration & Representativeness.
7. Country-level spread.
8. Global results.
9. World map.
10. Scenario lab.
11. Variant dynamics.

### Step 5 — Add new modules only after v17 is frozen
New modules should be added after the academic v17 is stable:

- `mod_qa_simulator.R`
- `mod_situational_awareness.R`
- `mod_will_it_spread.R`
- `mod_epidecision_room.R`

## Product separation

### Academic v17
Keep focused on:

- SEIRD;
- Scenario Lab;
- COVID reference comparator;
- maps;
- global curves;
- country-level spread;
- assumptions and limitations;
- export/import.

### Situational Awareness Lab
Add later as an analytical/teaching module:

- data scarcity;
- underascertainment;
- random sampling;
- when-to-measure logic;
- seroprevalence;
- wastewater surveillance;
- quasi-experiments;
- closed-setting transmission.

### Will it spread?
Gamified international containment module. Keep separate from academic v17.

### EpiDecision Room
Regional decision-making module. Requires new model structures for regions, health workforce, burnout, economic/social cost, and governance constraints.

## Immediate next task

Create `app_v17_freeze_candidate.R` from the current English app, then perform only low-risk edits:

1. update header/version metadata;
2. add a project architecture comment block;
3. identify visible UI strings;
4. create regression test plan;
5. do not modify model behavior yet.
