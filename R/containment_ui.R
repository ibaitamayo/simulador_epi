
containment_ui <- function(continents, countries) {

  tagList(
    h4(
      "Containment measures",
      style = "color: #7D3C98; font-weight: bold;"
    ),

    selectInput(
      "containment_preset",
      "Measure intensity:",
      choices = c(
        "No measures" = "none",
        "Mild" = "mild",
        "Moderate" = "moderate",
        "Strong" = "strong",
        "Custom" = "custom"
      ),
      selected = "none"
    ),

    selectInput(
      "containment_geographic_scope",
      "Where measures are applied:",
      choices = c(
        "Global: all countries" = "global",
        "By continent" = "continent",
        "By selected countries" = "countries"
      ),
      selected = "global"
    ),

    conditionalPanel(
      condition = "input.containment_geographic_scope == 'continent'",
      selectizeInput(
        "containment_affected_continents",
        "Affected continents:",
        choices = continents,
        selected = "Europe",
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = "input.containment_geographic_scope == 'countries'",
      selectizeInput(
        "containment_affected_countries",
        "Affected countries:",
        choices = countries,
        selected = "Norway",
        multiple = TRUE
      )
    ),

    numericInput(
      "containment_start_day",
      "Start day of measures:",
      value = 210,
      min = 0,
      max = 1095,
      step = 1
    ),

    numericInput(
      "containment_end_day",
      "End day of measures:",
      value = 240,
      min = 0,
      max = 1095,
      step = 1
    ),

    conditionalPanel(
      condition = "input.containment_preset == 'custom'",
      numericInput(
        "containment_transmission_reduction",
        "Local transmission reduction (%):",
        value = 50,
        min = 0,
        max = 100,
        step = 1
      ),
      numericInput(
        "containment_mobility_reduction",
        "Between-country mobility reduction (%):",
        value = 50,
        min = 0,
        max = 100,
        step = 1
      )
    ),

    conditionalPanel(
      condition = "input.active_scenario_mode == 'complete'",
      textInput(
        "containment_label",
        "Measure-period label:",
        value = "Containment period"
      )
    ),

    helpText(
      paste(
        "Definitions: Mild = partial reduction in transmission and mobility.",
        "Moderate = intermediate package.",
        "Strong = temporary high-intensity reduction.",
        "Measures apply only to the configurable simulation, not to the comparator."
      )
    ),

    hr()
  )
}

