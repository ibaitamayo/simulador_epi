# Regression Harness v17

Purpose:
Prevent silent regressions during refactoring.

Reference app:
app_epidemiologic_v17_academic_freeze.R

Canonical scenarios:
- baseline_no_containment
- early_moderate_containment
- strong_containment
- high_mobility_no_containment
- seird_E_sensitivity

Core KPIs:
- peak_active_population
- peak_day
- cumulative_deaths
- cumulative_recovered
- countries_reached
- exposed_peak
- comparator_loaded
- warnings_count
- runtime_seconds

Tolerance:
- numeric metrics: ±0.5%
- peak_day: ±1 day
- countries_reached: exact
- comparator_loaded: exact
- warnings_count: exact
