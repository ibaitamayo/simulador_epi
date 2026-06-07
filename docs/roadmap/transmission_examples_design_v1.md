# Transmission Examples Design v1

## Objective

Introduce literature-informed Transmission Examples while preserving the simplicity and accessibility of the simulator.

The feature should help users start from meaningful configurations without increasing the complexity of the default workflow.

---

## Design Principles

The user should be able to run a reasonable simulation without understanding:

- transmission families;
- behaviour modifiers;
- evidence hierarchies;
- implementation details.

The feature should provide useful defaults while remaining optional.

---

## User Experience Goals

A first-time user should be able to:

1. Select a country.
2. Select a Transmission Example.
3. Run a simulation.

No additional configuration should be required.

---

## User Interface

### Basic Mode

A single selector is displayed.

Transmission Example

- Generic
- COVID-19
- Influenza
- Measles
- Ebola
- Cholera
- Tuberculosis

Selecting an example automatically loads recommended starting values.

Users may immediately run the simulation.

---

### Advanced Mode

The same selector remains visible.

Additional controls allow users to modify any loaded parameter.

Once parameters are manually changed, the example should be marked as:

Custom

to indicate that the configuration no longer matches the original example.

---

## About this Example

An optional information panel should be available.

The panel remains collapsed by default.

The panel contains:

### Description

Short description of the example.

### Typical Characteristics

High-level qualitative summary.

Examples:

- respiratory transmission
- regional spread
- high transmission intensity
- slow progression

### References

Key literature sources.

### Evidence Level

- High
- Moderate
- Exploratory

### Confidence Level

- High
- Moderate
- Low

### Scope

Appropriate uses.

### Assumptions

Main modelling assumptions.

### Limitations

Important simplifications.

---

## Internal Architecture

User Selection

↓

Transmission Example

↓

Transmission Family

↓

Default Parameters

↓

Behaviour Settings

↓

Simulation Engine

Transmission Families remain internal concepts.

They are not exposed as primary user-facing controls.

---

## Comparator Integration

Transmission Examples and Reference Comparators are independent concepts.

Current Reference Comparator:

COVID-19 Omicron Reference Scenario

Loading a Transmission Example must never modify the Reference Comparator.

---

## Simplicity Safeguards

The following should not appear in Basic Mode:

- Transmission Families
- Behaviour Modifiers
- Evidence Metadata
- Technical Configuration

These concepts remain accessible through advanced documentation.

---

## Future Extensions

Potential future examples:

- SARS
- MERS
- Mpox
- Marburg
- 1918 Influenza
- Black Death

Future additions should not require changes to the simulation engine.

---

## Acceptance Criteria

A new user can:

- understand the purpose of the feature;
- select an example;
- run a simulation;

without reading technical documentation.

Advanced users can:

- inspect assumptions;
- inspect references;
- customize parameters;

without affecting simplicity for other users.


---

## Final UI Placement Requirement

In the final user interface, the Transmission Example selector should be placed immediately above the main epidemiological parameter controls, especially R0 and related model parameters.

Rationale:

- The example selector loads starting parameter values.
- Users should see the loaded parameters immediately below the selector.
- This reinforces that examples are editable starting configurations, not fixed models.

The current prototype placement is temporary and only validates UI availability.

---

## Custom State Detection

### Objective

Users should always know whether they are working with:

- an original Transmission Example; or
- a modified configuration derived from an example.

### Behaviour

When a Transmission Example is selected, the interface should display:

Status: Original

If the user modifies one or more parameters that belong to the example definition, the status should automatically change to:

Status: Custom

Tracked parameters include:

- R0
- Exposed period
- Infectious period
- Mortality
- future example-controlled parameters

### Reloading

Selecting the same example again should reload its stored defaults and restore:

Status: Original

### Rationale

Transmission Examples represent documented starting configurations.

Users remain free to modify parameters, but the interface should clearly distinguish between:

- the documented example; and
- a user-customised configuration.

This improves transparency, reproducibility and educational clarity.

### Implementation Note

The simulator should maintain:

loaded_example_id

and compare active parameter values against the currently loaded example definition.

If any tracked parameter differs from the stored example value, the example state becomes:

Custom
