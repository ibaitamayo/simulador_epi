
---

## Global Compartment Visualization Requirement

Transmission Examples may load either SIRD or SEIRD-compatible configurations.

The global compartment visualization must adapt to the active compartment model.

### SIRD mode

When the active model is SIRD, the global curves should display:

- Susceptible
- Infectious / active
- Recovered
- Deceased

The exposed compartment should not be displayed.

### SEIRD mode

When the active model is SEIRD, the global curves should display:

- Susceptible
- Exposed
- Infectious / active
- Recovered
- Deceased

The Exposed curve must be shown only when the active compartment model includes an exposed compartment.

### Implementation note

The Exposed curve should be conditional on:

active_compartment_model == "SEIRD"

or an equivalent internal model flag.

This requirement applies independently of the selected Transmission Example.

---

# Phase 1 Implementation Strategy

## Principle

Transmission Examples are parameter loaders.

They do not introduce new epidemiological models, new equations or new simulation engines.

Loading a Transmission Example simply populates existing simulator parameters with literature-informed defaults.

---

# Core Objects

## TRANSMISSION_EXAMPLES

Purpose:

Store the default parameter values associated with each example.

Example structure:

Generic
COVID-19
Influenza
Measles
Ebola
Cholera
Tuberculosis

Each example loads a predefined parameter set.

---

## TRANSMISSION_EXAMPLE_METADATA

Purpose:

Store explanatory and documentation information.

Metadata should not influence simulation calculations.

Metadata fields include:

- description
- references
- evidence_level
- confidence_level
- scope
- assumptions
- limitations
- version

---

## TRANSMISSION_FAMILIES

Purpose:

Internal classification system.

Transmission families are implementation concepts.

They are not displayed as primary user-facing controls.

Initial families:

- generic
- respiratory
- regional_contact
- water_associated
- slow_progression

---

# Example Schema

Each entry in TRANSMISSION_EXAMPLES should contain:

- id
- label
- family
- compartment_model
- default_R0
- default_exposed_period_days
- default_infectious_period_days
- default_mortality_percent
- default_dynamic_scenario
- default_starting_country
- enabled

---

# Metadata Schema

Each entry in TRANSMISSION_EXAMPLE_METADATA should contain:

- id
- description
- evidence_level
- confidence_level
- references
- scope
- assumptions
- limitations
- version
- last_review_date

---

# Family Schema

Each entry in TRANSMISSION_FAMILIES should contain:

- family_id
- family_label
- description

---

# Initial Example Mapping

COVID-19
→ respiratory

Influenza
→ respiratory

Measles
→ respiratory

Ebola
→ regional_contact

Cholera
→ water_associated

Tuberculosis
→ slow_progression

Generic
→ generic

---

# User Interface Integration

Basic Mode:

Transmission Example selector only.

Advanced Mode:

Transmission Example selector
+
full parameter editing.

Users may modify any loaded parameter.

---

# Custom State Detection

If the user changes one or more parameters after loading an example:

Example label:

Custom

should be displayed.

The original example remains available for reloading.

---

# Comparator Independence

Loading a Transmission Example must never modify:

- Reference Comparator
- Comparator RDS
- Comparator metadata

Reference Comparators remain independent from examples.

---

# Backward Compatibility

If no example is selected:

Generic

must be loaded automatically.

Existing simulations should continue to function without modification.

---

# Phase 2

Future work:

- richer metadata display
- evidence browser
- literature cards

No engine changes.

---

# Phase 3

Potential future extensions:

- seasonality modifiers
- mobility emphasis modifiers
- evolution potential modifiers

These features should remain optional and should not increase complexity in Basic Mode.

