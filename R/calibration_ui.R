
calibration_ui <- function(world_population) {

  conditionalPanel(
    condition = "input.active_scenario_mode == 'complete'",

    h4(
      "Representativeness and calibration",
      style = "color: #1F618D; font-weight: bold;"
    ),

    checkboxInput(
      "enable_calibration_targets",
      "Show calibration diagnostics",
      value = TRUE
    ),

    numericInput(
      "calibration_population",
      "Reference population for calibration:",
      value = world_population,
      min = 1,
      step = 1000000
    ),

    numericInput(
      "observed_positive_cumulative",
      "Observed cumulative positives (absolute number):",
      value = NA,
      min = 0,
      step = 100000
    ),

    numericInput(
      "case_detection_fraction",
      "User-specified detection fraction (0-1):",
      value = 0.20,
      min = 0.0001,
      max = 1.0,
      step = 0.01
    ),

    checkboxInput(
      "estimate_detection_from_serology",
      "Estimate detection fraction using seroprevalence and positives",
      value = TRUE
    ),

    numericInput(
      "observed_positive_day",
      "Observation day for cumulative positives:",
      value = 60,
      min = 0,
      max = 1095,
      step = 1
    ),

    numericInput(
      "positive_lag_days",
      "Lag from underlying event to positive record (days; default 5, plausible range 3-7):",
      value = 5,
      min = 0,
      max = 60,
      step = 1
    ),

    numericInput(
      "observed_seroprevalence_percent",
      "Observed seroprevalence (% of reference population):",
      value = NA,
      min = 0,
      max = 100,
      step = 0.1
    ),

    numericInput(
      "observed_seroprevalence_day",
      "Observation day for seroprevalence:",
      value = 90,
      min = 0,
      max = 1095,
      step = 1
    ),

    numericInput(
      "seroprevalence_lag_days",
      "Lag from underlying event to serology signal (days; default 26, plausible range 18-30):",
      value = 26,
      min = 0,
      max = 90,
      step = 1
    ),

    checkboxInput(
      "enable_day0_date_estimation",
      "Estimate calendar date for simulated day 0",
      value = FALSE
    ),

    dateInput(
      "observed_positive_calendar_date",
      "Calendar date of cumulative-positive observation:",
      value = NULL,
      format = "yyyy-mm-dd"
    ),

    dateInput(
      "observed_seroprevalence_calendar_date",
      "Calendar date of seroprevalence observation:",
      value = NULL,
      format = "yyyy-mm-dd"
    ),

    helpText(
      paste(
        "Diagnostic only:",
        "these inputs do not change the simulated trajectory.",
        "They compare positives and seroprevalence with",
        "lag-adjusted cumulative outputs and estimate",
        "an implied detection fraction."
      )
    ),

    hr()
  )
}

