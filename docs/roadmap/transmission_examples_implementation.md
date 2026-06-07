
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
