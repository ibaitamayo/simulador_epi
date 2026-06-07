reference_comparator_label <- function(ref_type) {

  dplyr::case_when(
    identical(ref_type, "age_adjusted_seird") ~ "Age-adjusted Omicron SEIRD RDS",
    identical(ref_type, "age_adjusted_sird") ~ "Age-adjusted Omicron SIRD RDS",
    identical(ref_type, "age_adjusted") ~ "Age-adjusted Omicron SIRD RDS",
    identical(ref_type, "basic") ~ "Basic Omicron SIRD RDS",
    TRUE ~ "Automatic comparator selection"
  )
}

reference_comparator_value <- function(value, default = "Not listed") {

  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  value <- value[[1]]

  if (is.null(value) || is.na(value)) {
    return(default)
  }

  as.character(value)
}

reference_comparator_cache_value <- function(
    ref_cache,
    candidate_names,
    default = "Not listed") {

  if (is.null(ref_cache)) {
    return(default)
  }

  search_spaces <- list(
    ref_cache,
    if (!is.null(ref_cache$config)) ref_cache$config else NULL,
    if (!is.null(ref_cache$cost_profile)) ref_cache$cost_profile else NULL,
    if (!is.null(ref_cache$parameters)) ref_cache$parameters else NULL,
    if (!is.null(ref_cache$metadata)) ref_cache$metadata else NULL
  )

  for (space in search_spaces) {
    if (is.null(space)) {
      next
    }

    for (candidate_name in candidate_names) {
      if (!is.null(space[[candidate_name]])) {
        return(reference_comparator_value(space[[candidate_name]], default = default))
      }
    }
  }

  default
}

reference_comparator_parameter_cards <- function(
    ref_type = "auto",
    ref_cache = NULL,
    ref_file = NULL,
    fallback_note = NULL) {

  ref_model <- if (!is.null(ref_cache$compartment_model)) {
    ref_cache$compartment_model
  } else if (identical(ref_type, "age_adjusted_seird")) {
    "SEIRD"
  } else {
    "SIRD"
  }

  comparator_rows <- data.frame(
    parameter = c(
      "Reference profile",
      "Comparator selection",
      "Model structure",
      "Comparator file",
      "Initial country",
      "R0 / Rt starting value",
      "Exposed period",
      "Active / infectious period",
      "Average mortality",
      "Purpose"
    ),
    value = c(
      "COVID-19 Omicron",
      reference_comparator_label(ref_type),
      reference_comparator_value(ref_model),
      reference_comparator_value(ref_file),
      reference_comparator_cache_value(
        ref_cache,
        c("initial_country", "initial_country_name", "country", "origin_country"),
        default = "South Africa"
      ),
      reference_comparator_cache_value(
        ref_cache,
        c("R0", "r0", "default_R0", "baseline_R0"),
        default = "Not listed"
      ),
      reference_comparator_cache_value(
        ref_cache,
        c(
          "exposed_period_days",
          "exposed_period",
          "default_exposed_period_days",
          "incubation_period_days",
          "latent_period_days"
        ),
        default = if (identical(ref_model, "SEIRD")) "4 days" else "Not applicable to SIRD"
      ),
      reference_comparator_cache_value(
        ref_cache,
        c(
          "infectious_period_days",
          "active_infectious_period_days",
          "infectious_period",
          "default_infectious_period_days"
        ),
        default = "5 days"
      ),
      reference_comparator_cache_value(
        ref_cache,
        c(
          "mortality_rate",
          "mortality_percent",
          "default_mortality_percent",
          "average_mortality_percent"
        ),
        default = "1%"
      ),
      "Static benchmark for visual comparison; not modified by Transmission Examples or containment settings."
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(fallback_note) && nzchar(fallback_note)) {
    comparator_rows <- rbind(
      comparator_rows,
      data.frame(
        parameter = "Fallback note",
        value = fallback_note,
        stringsAsFactors = FALSE
      )
    )
  }

  cards <- lapply(seq_len(nrow(comparator_rows)), function(i) {
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
        comparator_rows$parameter[i]
      ),
      tags$div(
        style = "font-size:12px; color:#555;",
        comparator_rows$value[i]
      )
    )
  })

  tags$div(
    style = "margin-top:8px;",
    cards
  )
}

reference_comparator_info_content <- function(
    ref_type = "auto",
    ref_cache = NULL,
    ref_file = NULL,
    fallback_note = NULL) {

  tags$div(
    tags$div(
      style = paste(
        "margin:8px 0 12px 0;",
        "padding:8px 10px;",
        "background-color:#F4F8FB;",
        "border-left:4px solid #2E86C1;",
        "font-size:12px;",
        "color:#555;"
      ),
      "Static COVID-19 Omicron comparator. ",
      "It is used only as a fixed visual benchmark and is independent from editable Transmission Examples."
    ),

    tags$details(
      style = "margin-top:10px;",
      tags$summary(
        style = paste(
          "cursor:pointer;",
          "color:#2E86C1;",
          "font-weight:700;"
        ),
        "View comparator parameters"
      ),
      reference_comparator_parameter_cards(
        ref_type = ref_type,
        ref_cache = ref_cache,
        ref_file = ref_file,
        fallback_note = fallback_note
      )
    )
  )
}
