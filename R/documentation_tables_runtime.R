update_default_assumptions_registry <- function(registry_template, input) {

  registry <- registry_template

  set_value <- function(parameter_key, value) {
    idx <- registry$Parameter == parameter_key
    if (any(idx)) {
      registry$Current_value[idx] <<- as.character(value)
    }
  }

  set_value("R0_input", isolate(input$R0))
  set_value("infectious_window_days", isolate(input$infectious_period_days))
  set_value("infectiousness_profile", isolate(input$infectiousness_profile))
  set_value("mortality_rate_default_guided", paste0(isolate(input$mortality_rate), "%"))
  set_value("case_detection_fraction", isolate(input$case_detection_fraction))
  set_value("positive_lag_days", isolate(input$positive_lag_days))
  set_value("seroprevalence_lag_days", isolate(input$seroprevalence_lag_days))
  set_value("air_travel_scenario", isolate(input$air_travel_scenario))
  set_value("import_establishment_probability", isolate(input$import_establishment_probability))
  set_value("containment_geographic_scope", isolate(input$containment_geographic_scope))
  set_value("mutation_rate_per_replication", isolate(input$mutation_rate_per_replication))
  set_value("effective_mutation_targets", isolate(input$effective_mutation_targets))

  registry
}
