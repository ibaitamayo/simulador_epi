# Mobility Model

## Purpose

EPIDEM incorporates international mobility to represent opportunities for geographic dissemination between countries.

The objective is to approximate large-scale connectivity patterns while maintaining computational efficiency.

---

## Conceptual approach

Mobility is represented at population level.

The model does not simulate individual travellers, routes, flights, or transportation networks.

Instead, mobility is represented through aggregated connectivity between countries.

---

## Passenger matrix

International connectivity is represented through a bilateral passenger matrix.

Each matrix element represents the relative intensity of movement between two countries.

The matrix acts as a proxy for opportunities for geographic dissemination.

---

## Geographic dissemination

Geographic dissemination is influenced by:

- transmission intensity in the origin country;
- remaining infectious pressure;
- passenger connectivity;
- scenario-specific modifiers.

Countries with stronger connectivity experience greater exposure to imported transmission pressure.

---

## Mobility and containment

Containment measures may modify mobility intensity.

This allows exploration of scenarios involving:

- reduced international movement;
- delayed dissemination;
- heterogeneous regional connectivity.

---

## Interpretation

Mobility should be interpreted as a population-level approximation of connectivity.

It is not intended to reproduce specific transportation systems or predict actual passenger behaviour.

---

## Computational design

The mobility model is designed to:

- remain computationally efficient;
- support multi-country simulation;
- allow rapid scenario exploration;
- remain interpretable.

The objective is to balance realism and transparency.

---

## Limitations

The current implementation:

- uses aggregated mobility assumptions;
- does not simulate individual travel events;
- does not represent transportation networks explicitly;
- does not include behavioural adaptation by travellers.

These simplifications are intentional and consistent with the educational and planning objectives of EPIDEM.

