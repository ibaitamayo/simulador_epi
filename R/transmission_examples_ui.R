
transmission_example_status_badge <- function(example_state) {

  if (!identical(example_state, "Custom")) {
    return(NULL)
  }

  tags$div(
    style = paste(
      "margin-top:6px;",
      "padding:4px 8px;",
      "border-radius:6px;",
      "background-color:#FDEBD0;",
      "color:#7E5109;",
      "font-weight:600;"
    ),
    "Status: Custom — aggregate transmission parameters modified"
  )
}

transmission_example_info_panel <- function() {

  tags$details(
    style = "margin-top: 10px;",
    tags$summary(
      style = paste(
        "cursor:pointer;",
        "color:#2E86C1;",
        "font-weight:700;",
        "background-color:#EBF5FB;",
        "padding:6px 8px;",
        "border-radius:6px;"
      ),
      "About this Example"
    ),
    tags$div(
      style = paste(
        "background-color:#F8FBFD;",
        "border-left:3px solid #2E86C1;",
        "padding:8px 10px;",
        "margin-top:6px;",
        "border-radius:4px;"
      ),
      uiOutput("transmission_example_info")
    )
  )
}



transmission_example_reference_basis <- function(reference_type) {

  if (grepl("peer_reviewed", reference_type, ignore.case = TRUE)) {
    return("Peer reviewed")
  }

  if (grepl("institutional", reference_type, ignore.case = TRUE)) {
    return("Institutional")
  }

  if (grepl("preprint", reference_type, ignore.case = TRUE)) {
    return("Preprint")
  }

  "Other"
}

transmission_example_parameter_sources_table <- function(
    example_id,
    examples = TRANSMISSION_EXAMPLES,
    references = TRANSMISSION_EXAMPLE_REFERENCES) {

  example_row <- examples[examples$id == example_id, ]

  if (nrow(example_row) != 1) {
    return(NULL)
  }

  parameter_map <- data.frame(
    parameter_key = c(
      "default_R0",
      "default_exposed_period_days",
      "default_infectious_period_days",
      "default_mortality_percent"
    ),
    parameter_label = c(
      "R0 / Rt starting value",
      "Exposed period",
      "Active / infectious period",
      "Average mortality"
    ),
    unit = c(
      "unitless",
      "days",
      "days",
      "%"
    ),
    stringsAsFactors = FALSE
  )

  source_cards <- lapply(seq_len(nrow(parameter_map)), function(i) {

    parameter_key <- parameter_map$parameter_key[i]
    parameter_label <- parameter_map$parameter_label[i]
    unit <- parameter_map$unit[i]
    value <- as.character(example_row[[parameter_key]])

    parameter_refs <- references[
      references$example_id == example_id &
        grepl(parameter_key, references$reference_type, fixed = TRUE),
    ]

    reference_items <- if (nrow(parameter_refs) == 0) {
      tags$div(
        style = "font-size:12px; color:#666; margin-top:4px;",
        "No parameter-level reference listed."
      )
    } else {
      tags$ul(
        style = "padding-left:18px; margin:4px 0 0 0;",
        lapply(seq_len(nrow(parameter_refs)), function(j) {
          tags$li(
            style = "font-size:12px; margin-bottom:4px;",
            tags$span(
              style = "font-weight:600;",
              transmission_example_reference_basis(parameter_refs$reference_type[j])
            ),
            ": ",
            parameter_refs$reference_label[j]
          )
        })
      )
    }

    tags$div(
      style = paste(
        "margin-top:8px;",
        "padding:8px;",
        "background-color:#FFFFFF;",
        "border:1px solid #D6EAF8;",
        "border-radius:4px;"
      ),
      tags$div(
        style = "font-weight:700; color:#2C3E50;",
        parameter_label
      ),
      tags$div(
        style = "font-size:12px; color:#555;",
        paste0("Current value: ", value, " ", unit)
      ),
      reference_items
    )
  })

  tags$div(
    style = "margin-top:8px;",
    source_cards
  )
}

transmission_example_info_content <- function(
    example_id,
    examples = TRANSMISSION_EXAMPLES,
    metadata = TRANSMISSION_EXAMPLE_METADATA,
    references = TRANSMISSION_EXAMPLE_REFERENCES) {

  md <- get_transmission_example_metadata(
    example_id,
    metadata = metadata
  )

  if (is.null(md)) {
    return(NULL)
  }

  tags$div(
    tags$div(
      style = "margin-top:8px;",
      tags$b("Evidence status"),
      tags$p(md$evidence_level)
    ),

    tags$div(
      style = paste(
        "margin:8px 0 12px 0;",
        "padding:8px 10px;",
        "background-color:#FFF8E1;",
        "border-left:4px solid #F0AD4E;",
        "font-size:12px;",
        "color:#555;"
      ),
      "Literature-informed starting configuration for aggregate simulation. ",
      "Values are editable and remain review_pending until formal parameter review. ",
      "Use “View parameter values and sources” to inspect parameter-level references."
    ),

    tags$div(
      tags$b("Description"),
      tags$p(md$description)
    ),

    tags$div(
      tags$b("Scope"),
      tags$p(md$scope)
    ),

    tags$div(
      tags$b("Limitations"),
      tags$p(md$limitations)
    ),

    tags$details(
      style = "margin-top:10px;",
      tags$summary(
        style = paste(
          "cursor:pointer;",
          "color:#2E86C1;",
          "font-weight:700;"
        ),
        "View parameter values and sources"
      ),
      transmission_example_parameter_sources_table(
        example_id = example_id,
        examples = examples,
        references = references
      )
    )
  )
}
