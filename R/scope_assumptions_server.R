register_scope_assumptions_outputs <- function(
    output,
    input,
    default_assumptions_registry_template,
    default_bibliography_table,
    world_polygon_available_fn,
    selected_fixed_reference_type_fn,
    selected_fixed_reference_file_fn,
    selected_fixed_reference_cache_fn,
    get_dynamic_config_fn) {

  output$deployment_checklist_table <- renderTable({
    data.frame(
      Item = c(
        "Country age distribution RDS",
        "Country polygon RDS",
        "SEIRD COVID comparator RDS",
        "Runtime RDS writing",
        "Map animation interval",
        "Default containment window",
        "Scenario laboratory persistence"
      ),
      Expected = c(
        "country_age_distribution_wpp2024_6groups.rds available in deployment environment",
        "world_countries_simplified.rds available in deployment environment",
        "fixed_covid_omicron_reference_age_adjusted_seird.rds available in deployment environment",
        "Disabled inside Shiny app",
        "100 ms",
        "Start day 210; end day 240 unless a preset changes it",
        "Session-level; export/import JSON for reuse across sessions"
      ),
      Status = c(
        ifelse(
          file.exists(AGE_DISTRIBUTION_RDS_FILE) ||
            file.exists(AGE_DISTRIBUTION_CSV_FILE),
          "available",
          "fallback or missing"
        ),
        ifelse(world_polygon_available_fn(), "available", "fallback markers only"),
        ifelse(identical(selected_fixed_reference_type_fn(), "age_adjusted_seird"), "available / selected", "not selected or missing"),
        "ok",
        "ok",
        "ok",
        "session-level"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$default_assumptions_registry <- renderTable({
    update_default_assumptions_registry(default_assumptions_registry_template, input)
  }, rownames = FALSE)

  output$default_bibliography_table <- renderTable({
    default_bibliography_table
  }, rownames = FALSE)

  output$seird_assumptions_table <- renderTable({
    data.frame(
      Item = c(
        "Compartment structure",
        "Meaning of E",
        "Default E duration",
        "Why 4 days?",
        "Plausible sensitivity range",
        "Interpretation caution",
        "Comparator alignment"
      ),
      Current_assumption = c(
        "Guided mode uses SEIRD; SIRD remains available in Complete advanced mode.",
        "E is the exposed / pre-active compartment: already in the modelled infection process but not yet contributing to active-transmission pressure.",
        "4 days from exposure to active compartment.",
        "Chosen as a pragmatic Omicron-like default: published estimates place Omicron incubation commonly around 3-4 days, and one Omicron transmission-dynamics study estimated mean incubation 3.8 days and latent period 3.1 days.",
        "3-5 days for Omicron-like scenarios; longer values can be explored for earlier-lineage-like or slower-onset assumptions.",
        "The app's E delay is a modelling delay, not a direct clinical incubation estimate; it is used to delay and smooth entry into the active compartment.",
        "When available, fixed_covid_omicron_reference_age_adjusted_seird.rds is used so the active scenario and COVID comparator share the SEIRD structure."
      ),
      Source_or_basis = c(
        "Internal modelling architecture",
        "Standard SEIR/SEIRD compartment logic",
        "App default",
        "Xu et al. 2023; Xin et al. 2023; Liu et al. 2022; CDC EID BA.5 analysis",
        "Literature-informed sensitivity range",
        "Model-scope statement",
        "Runtime comparator selection"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$dynamic_assumptions_table <- renderTable({
    cfg <- get_dynamic_config_fn()
    data.frame(
      Parameter = c(
        "Dynamic scenario",
        "Enabled",
        "Macro-effective opportunity rate",
        "Population-level replacement opportunity multiplier",
        "Calibration level",
        "Maximum R0 multiplier",
        "Preset interpretation",
        "Targets definition"
      ),
      Value = c(
        cfg$scenario_label,
        as.character(cfg$enabled),
        as.character(cfg$rate),
        as.character(cfg$targets),
        as.character(cfg$calibration),
        as.character(cfg$max_multiplier),
        ifelse(is.null(cfg$interpretation), "Population-level replacement dynamics; reviewed conservative guided preset", cfg$interpretation),
        "Abstract count of transmission-relevant macro opportunities used to scale candidate generation; not a genomic site count"
      ),
      Status = c(
        "scenario preset",
        "scenario preset",
        "scenario preset / needs calibration",
        "scenario preset / needs calibration",
        "scenario preset / needs calibration",
        "scenario cap / literature-informed",
        "scope statement",
        "definition"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$reference_comparator_metadata_table <- renderTable({
    ref_file <- tryCatch(selected_fixed_reference_file_fn(), error = function(e) NA_character_)
    ref_type <- tryCatch(selected_fixed_reference_type_fn(), error = function(e) "basic")
    ref_cache <- tryCatch(selected_fixed_reference_cache_fn(), error = function(e) NULL)
    ref_model <- if (!is.null(ref_cache$compartment_model)) ref_cache$compartment_model else ifelse(identical(ref_type, "age_adjusted_seird"), "SEIRD", "SIRD")
    ref_exposed <- if (!is.null(ref_cache$exposed_period_days)) ref_cache$exposed_period_days else ifelse(identical(ref_model, "SEIRD"), 4, 0)

    data.frame(
      Field = c(
        "Comparator role",
        "RDS file",
        "Compartment model",
        "Exposed compartment E",
        "Origin",
        "R0",
        "Active window",
        "Mortality parameter",
        "Dynamic module",
        "Containment included",
        "Runtime behavior"
      ),
      Value = c(
        "Reference shown against active scenario",
        basename(ref_file),
        ref_model,
        ifelse(identical(ref_model, "SEIRD"), paste0("Included; ", ref_exposed, " days from exposure to active phase"), "Not included"),
        "South Africa",
        "4.25",
        "5 days",
        "1% in current comparator",
        "Disabled in comparator",
        "No",
        "Read from RDS; sliced to selected duration; not recalculated when the active simulation runs"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)
}
