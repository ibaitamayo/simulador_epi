update_default_assumptions_registry <- function(registry_template, input) {

  registry <- registry_template

  set_value <- function(parameter, value) {
    idx <- registry$Parameter == parameter
    if (any(idx)) {
      registry$Default_or_current_value[idx] <<- as.character(value)
    }
  }

  set_value("R0", isolate(input$R0))
  set_value("Infectious period / active window", isolate(input$infectious_period_days))
  set_value("Infectiousness profile", isolate(input$infectiousness_profile))
  set_value("Global mortality input", paste0(isolate(input$mortality_rate), "%"))
  set_value("Case detection fraction", isolate(input$case_detection_fraction))
  set_value("Positive-record lag", isolate(input$positive_lag_days))
  set_value("Seroprevalence lag", isolate(input$seroprevalence_lag_days))
  set_value("Air travel scenario", isolate(input$air_travel_scenario))
  set_value("Import establishment probability", isolate(input$import_establishment_probability))
  set_value("Containment scope", isolate(input$containment_geographic_scope))
  set_value("Dynamic rate", isolate(input$mutation_rate_per_replication))
  set_value("Dynamic effective targets", isolate(input$effective_mutation_targets))

  registry
}
