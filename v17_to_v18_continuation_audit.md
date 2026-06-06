# Shiny epidem v17→v18 continuation audit

## Scope

Audited English source file: `app_epidemiologic_v16_scenario_lab_step6b_basic_exposed_parameter.R` (6,633 lines). The goal is to continue development on the English version, protect current behavior, and prepare modular extraction without translating or renaming programmatic identifiers.

## Immediate decision

Continue in R/Shiny. Do not move to Python or mobile at this stage. The correct next step is a controlled refactor:

1. freeze an English academic v17 baseline;
2. create regression outputs for a small set of canonical scenarios;
3. extract low-risk pure functions first;
4. only then extract UI modules;
5. add new modules such as `Situational Awareness Lab` after the baseline is stable.

## Current source structure

Approximate function inventory: 35 function definitions.

High-level blocks detected:

- Lines 1–781: libraries, constants, country metadata, passenger edges.
- Lines 782–1679: utility, mobility, age adjustment, containment, diagnostics.
- Lines 1848–2535: main UI definition.
- Lines 2539–4207: server helpers, reports, calibration, scenario helpers.
- Lines 4208–5159: simulation engine.
- Lines 5160–5662: reference comparator loading and simulation orchestration.
- Lines 5663–6149: global plots and maps.
- Lines 6150–6632: scenario outputs, country outputs, dynamic outputs, downloads.

## Refactor order

### Step 0 — no behavior change

Create an English v17 file from the current v16 source. Do not change logic. Only update header/version metadata and add comments indicating planned modularization.

### Step 1 — regression baseline

Create a deterministic baseline using fixed seed and canonical scenarios. Store outputs as CSV/RDS snapshots. Minimum scenarios:

1. Spain, SEIRD, E=4, no containment.
2. Spain, SEIRD, E=4, early moderate containment.
3. South Africa, COVID comparator-compatible configuration.
4. High mobility without containment.
5. SIRD sensitivity version of scenario 1.

Minimum metrics to compare after each refactor:

- peak active day;
- peak active;
- peak exposed and day;
- final deaths;
- final recovered;
- countries reached;
- top 10 countries;
- first reached day by country;
- scenario summary row.

### Step 2 — low-risk extraction

Extract pure helper functions and constants. Do not extract server-bound scenario functions yet.

Suggested first files:

- `R/utils_formatting.R`
- `R/data_catalog.R`
- `R/diagnostics.R`
- `R/containment.R`
- `R/ui_text_registry.R`

### Step 3 — medium-risk extraction

Extract age adjustment, mobility and reference comparator loading. Run the full baseline after each file is extracted.

Suggested files:

- `R/age_adjustment.R`
- `R/mobility.R`
- `R/reference_comparator.R`

### Step 4 — high-risk extraction

Only after baselines pass:

- `R/model_seird.R`
- `modules/mod_scenario_lab.R`
- `modules/mod_world_map.R`

## Agent usefulness assessment

An automated coding agent is useful, but only with strict boundaries.

### Good uses for an agent

- Extract visible UI strings into a registry.
- Build inventories of inputs, outputs and functions.
- Move pure functions into separate files without changing content.
- Generate regression harness scripts.
- Run repeated baseline comparisons.
- Detect accidental translation or renaming of identifiers.
- Produce documentation tables.

### Bad uses for an agent

- Rewriting the simulation engine semantically.
- Translating the app automatically.
- Renaming inputs/outputs.
- Refactoring leaflet maps without tests.
- Changing `sim_data`, `scenario_library`, passenger matrices or RDS loading without explicit review.
- Making broad search-and-replace operations.

### Recommended agent mode

Use an agent as a **bounded refactor assistant**, not as an autonomous product builder. Each task should be atomic:

- one file extraction;
- one test harness;
- one UI registry pass;
- one module skeleton.

Require a diff and a regression comparison after every task.

## New module placement

The future `Situational Awareness Lab` should not be inserted directly into the current monolithic server. First create independent logic files:

- `R/situational_awareness_metrics.R`
- `R/situational_awareness_sampling.R`
- `R/situational_awareness_quasiexperiments.R`
- `modules/mod_situational_awareness.R`

Visible UI strings should be registered from the start because final Spanish translation will use only visible literals.

## Files generated in this audit

- `refactor_inventory_v17.csv`
- `module_extraction_plan_v17_to_v18.csv`
- `ui_visible_strings_audit_v17.csv`
