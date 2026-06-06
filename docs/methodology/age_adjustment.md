# Age Adjustment Methodology

## Purpose

EPIDEM supports age-adjusted simulations to improve realism while preserving computational efficiency.

The objective is to account for differences in demographic structure between countries without moving to an individual-based simulation framework.

---

## Demographic inputs

Country-specific age distributions are derived from external demographic datasets.

The current implementation uses six aggregated age groups.

These distributions are incorporated into the simulation before execution.

---

## Rationale

Disease impact is not uniformly distributed across age groups.

Population age structure may influence:

- severity;
- mortality;
- recovery patterns;
- aggregate outcomes.

Countries with different demographic profiles may therefore experience different simulated outcomes even under identical transmission assumptions.

---

## Age groups

The model operates using aggregated age strata rather than individual ages.

Age groups are used exclusively for population-level adjustment and are not intended to represent individual trajectories.

---

## Mortality adjustment

Age distributions are incorporated into mortality calculations through weighted aggregation.

The objective is to approximate demographic differences while maintaining model simplicity.

---

## Computational design

Age adjustment is implemented at country level.

The approach avoids the computational cost associated with microsimulation or agent-based modelling.

This design allows rapid scenario exploration while incorporating demographic heterogeneity.

---

## Interpretation

Age-adjusted results should be interpreted as population-level approximations.

The model does not attempt to predict outcomes for specific individuals or patient groups.

---

## Limitations

The current implementation:

- uses aggregated age categories;
- assumes homogeneous behaviour within each age group;
- does not model age-specific contact networks;
- does not model individual risk factors.

These simplifications are intentional and support transparency and computational performance.

