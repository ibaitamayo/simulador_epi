# Scenario-related helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

scenario_safe_value <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0) return(default)
    x
  }


parse_optional_seed <- function(seed_value) {
    if (is.null(seed_value) || length(seed_value) == 0 || is.na(seed_value)) return(NULL)
    seed_value <- trimws(as.character(seed_value))
    if (!nzchar(seed_value)) return(NULL)
    parsed <- suppressWarnings(as.integer(seed_value))
    if (is.na(parsed)) return(NULL)
    parsed
  }


dynamic_preset_values <- function(preset) {
    preset <- if (is.null(preset) || !nzchar(preset)) "reference" else preset
    if (identical(preset, "off")) {
      return(list(enabled = FALSE, scenario = "off", scenario_label = "Off", rate = 0, targets = 30, calibration = "conservative", max_multiplier = 1.0, adaptive_saturation = FALSE, saturation_exponent = 1.0, interpretation = "Off: no dynamic replacement module."))
    }
    if (identical(preset, "low")) {
      return(list(enabled = TRUE, scenario = "low", scenario_label = "Low opportunity pressure", rate = 1e-7, targets = 20, calibration = "conservative", max_multiplier = 1.10, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "Low: expected to generate few or no established replacement events in many runs; adaptive-space saturation is active."))
    }
    if (identical(preset, "high")) {
      return(list(enabled = TRUE, scenario = "high", scenario_label = "High opportunity pressure", rate = 2e-6, targets = 50, calibration = "high", max_multiplier = 1.65, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "High: stress-test scenario with more population-level opportunities; adaptive-space saturation limits marginal gains near the cap."))
    }
    list(enabled = TRUE, scenario = "reference", scenario_label = "Reference opportunity pressure", rate = 5e-7, targets = 30, calibration = "reference", max_multiplier = 1.35, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "Reference: exploratory moderate population-level opportunity pressure; adaptive-space saturation is active.")
  }

