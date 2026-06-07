scope_assumptions_limitations_tab <- function() {
  tabPanel(
    "Scope, assumptions & limitations",

    h4("What this tool does"),
    tags$ul(
      tags$li("Runs a macroscopic population-level SIRD/SEIRD simulation for teaching, planning and visualization."),
      tags$li("Compares the active user scenario with a COVID comparator loaded from RDS."),
      tags$li("Explores aggregate geographic spread through a passenger-mobility matrix."),
      tags$li("Allows age-group CFR weighting, containment scenarios, calibration diagnostics and map-based visualization."),
      tags$li("Estimates diagnostic gaps between simulated cumulative outputs, observed positives and seroprevalence inputs."),
      tags$li("Reports computational cost, unresolved active-state diagnostics and copy-paste evaluation summaries.")
    ),

    h4("What this tool does not do"),
    tags$ul(
      tags$li("It does not predict future real-world case counts."),
      tags$li("It does not replace official calibrated models or formal statistical analyses."),
      tags$li("It does not infer causal effects of public policy by itself."),
      tags$li("It does not use individual-level contacts, clinical heterogeneity or real-time surveillance data unless entered manually."),
      tags$li("It does not automatically estimate optimal parameters."),
      tags$li("It does not provide an exact historical reconstruction."),
      tags$li("It does not recalculate or modify the COVID RDS comparator when the configurable simulation is run.")
    ),

    hr(),
    h4("Deployment and stability checklist"),
    tableOutput("deployment_checklist_table"),

    hr(),
    h4("Default assumptions registry"),
    tableOutput("default_assumptions_registry"),

    hr(),
    h4("Bibliographic sources for current defaults"),
    tableOutput("default_bibliography_table"),
    tags$p(
      class = "small-note",
      "Links are provided when a stable public source is available. Scenario presets remain labelled as such when the literature supports a range rather than a single universal value."
    ),

    hr(),
    h4("SEIRD and exposed-compartment assumptions"),
    tableOutput("seird_assumptions_table"),
    tags$p(
      class = "small-note",
      "The E compartment is a population-level timing state. It represents the delay between modelled exposure/infection and entry into the active transmitting compartment; it is not an individual diagnosis or laboratory state."
    ),

    hr(),
    h4("Dynamic module assumptions"),
    tableOutput("dynamic_assumptions_table"),
    tags$p(
      class = "small-note",
      "Guided dynamic presets are scenario settings. Adaptive-space saturation is active for guided dynamic presets: expressed advantage is reduced as the effective R0 approaches the scenario cap. Detailed parameters remain in Complete advanced mode and are labelled as technical defaults pending dedicated literature review."
    ),

    hr(),
    h4("Reference comparator metadata"),
    tableOutput("reference_comparator_metadata_table"),

    hr(),
    h4("Copy-paste scope report"),
    verbatimTextOutput("model_scope_report")
  )
}
