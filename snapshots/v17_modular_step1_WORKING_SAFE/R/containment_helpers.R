if (!exists("CONTINENTS_LIST")) {
  CONTINENTS_LIST <- character(0)
}

# Containment-related helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

make_containment_preset_table <- function() {
  data.frame(
    preset = c("none", "mild", "moderate", "strong", "custom"),
    label = c("None", "Mild", "Moderate", "Strong", "Custom"),
    transmission_reduction_percent = c(0, 25, 50, 75, NA_real_),
    mobility_reduction_percent = c(0, 20, 50, 75, NA_real_),
    definition = c(
      "No scenario-level reduction. Transmission and mobility multipliers remain 1.00.",
      "Mild: partial reduction in contacts and mobility. Default transmission reduction 25%; mobility reduction 20%.",
      "Moderate: intermediate package of measures. Default transmission reduction 50%; mobility reduction 50%.",
      "Strong: high-intensity temporary reduction in contacts and mobility. Default transmission reduction 75%; mobility reduction 75%.",
      "Custom: user-defined reductions for transmission and mobility."
    ),
    stringsAsFactors = FALSE
  )
}





containment_multipliers_for_day <- function(containment_schedule, day) {
  if (is.null(containment_schedule) || !isTRUE(containment_schedule$enabled)) {
    return(list(transmission = 1, mobility = 1, active = FALSE))
  }
  is_active <- is.finite(containment_schedule$start_day) && is.finite(containment_schedule$end_day) &&
    day >= containment_schedule$start_day && day <= containment_schedule$end_day
  if (!isTRUE(is_active)) {
    return(list(transmission = 1, mobility = 1, active = FALSE))
  }
  list(
    transmission = containment_schedule$transmission_multiplier,
    mobility = containment_schedule$mobility_multiplier,
    active = TRUE
  )
}


containment_is_active_for_day <- function(containment_schedule, day) {
  !is.null(containment_schedule) && isTRUE(containment_schedule$enabled) &&
    is.finite(containment_schedule$start_day) && is.finite(containment_schedule$end_day) &&
    day >= containment_schedule$start_day && day <= containment_schedule$end_day
}


containment_transmission_vector_for_day <- function(containment_schedule, day, countries) {
  out <- setNames(rep(1, length(countries)), countries)
  if (!containment_is_active_for_day(containment_schedule, day)) return(out)
  affected <- intersect(containment_schedule$affected_countries, countries)
  if (length(affected) == 0) return(out)
  out[affected] <- containment_schedule$transmission_multiplier
  out
}


apply_containment_to_passenger_matrix <- function(passenger_matrix_daily, containment_schedule, day, countries) {
  m <- passenger_matrix_daily
  if (!containment_is_active_for_day(containment_schedule, day)) return(m)
  affected <- intersect(containment_schedule$affected_countries, countries)
  if (length(affected) == 0) return(m)
  affected_idx <- rownames(m) %in% affected | colnames(m) %in% affected
  route_mask <- outer(rownames(m) %in% affected, colnames(m) %in% affected, `|`)
  m[route_mask] <- m[route_mask] * containment_schedule$mobility_multiplier
  m
}


format_containment_schedule_for_display <- function(containment_schedule) {
  if (is.null(containment_schedule)) return(data.frame(Metric = "Containment", Value = "Not available"))
  data.frame(
    Metric = c(
      "Enabled",
      "Preset",
      "Explicit definition",
      "Geographic scope",
      "Affected continents",
      "Affected country count",
      "Affected countries",
      "Mobility application rule",
      "Start day",
      "End day",
      "Days affected",
      "Transmission reduction",
      "Mobility reduction",
      "Transmission multiplier",
      "Mobility multiplier",
      "Applies to",
      "Reference Omicron RDS modified"
    ),
    Value = c(
      as.character(containment_schedule$enabled),
      containment_schedule$preset_label,
      containment_schedule$definition,
      containment_schedule$geographic_scope,
      paste(containment_schedule$affected_continents, collapse = ";"),
      as.character(containment_schedule$affected_country_count),
      paste(head(containment_schedule$affected_countries, 20), collapse = ";"),
      containment_schedule$mobility_application_rule,
      as.character(containment_schedule$start_day),
      as.character(containment_schedule$end_day),
      as.character(containment_schedule$active_days),
      paste0(sprintf("%.1f", containment_schedule$transmission_reduction_percent), "%"),
      paste0(sprintf("%.1f", containment_schedule$mobility_reduction_percent), "%"),
      sprintf("%.3f", containment_schedule$transmission_multiplier),
      sprintf("%.3f", containment_schedule$mobility_multiplier),
      containment_schedule$applies_to,
      as.character(containment_schedule$fixed_reference_modified)
    ),
    stringsAsFactors = FALSE
  )
}


calculate_horizon_diagnostics <- function(sim_result, mortality_rate = NA_real_, world_population = WORLD_POPULATION) {
  if (is.null(sim_result) || is.null(sim_result$data)) {
    return(data.frame(Metric = "Horizon diagnostic", Value = "Not available", stringsAsFactors = FALSE))
  }
  d <- sim_result$data
  final_active <- as.numeric(tail(d$I, 1))
  final_deaths <- as.numeric(tail(d$D, 1))
  pct_population <- if (is.finite(world_population) && world_population > 0) final_active / world_population else NA_real_

  if (!is.null(sim_result$age_summary) && nrow(sim_result$age_summary) > 0) {
    additional <- sum(as.numeric(sim_result$age_summary$active_final) * as.numeric(sim_result$age_summary$cfr), na.rm = TRUE)
  } else {
    additional <- final_active * max(0, min(1, as.numeric(mortality_rate)))
  }
  projected_total <- final_deaths + additional

  warning <- "none"
  final_active_rounded <- if (is.finite(final_active)) round(final_active) else NA_real_
  if (is.finite(final_active_rounded) && final_active_rounded <= 0) {
    warning <- "none"
  } else if (is.finite(pct_population) && pct_population >= 0.001) {
    warning <- "substantial_unresolved_active_at_end_of_simulation"
  } else if (is.finite(final_active_rounded) && final_active_rounded > 0) {
    warning <- "unresolved_active_at_end_of_simulation"
  }

  data.frame(
    Metric = c(
      "final_active",
      "final_active_percent_population",
      "final_active_warning",
      "final_deaths_simulated",
      "projected_additional_deaths_if_active_resolved",
      "projected_final_deaths_if_active_resolved",
      "interpretation_note"
    ),
    Value = c(
      format(round(final_active), scientific = FALSE, big.mark = ","),
      ifelse(is.na(pct_population), NA, sprintf("%.4f%%", 100 * pct_population)),
      warning,
      format(round(final_deaths), scientific = FALSE, big.mark = ","),
      format(round(additional), scientific = FALSE, big.mark = ","),
      format(round(projected_total), scientific = FALSE, big.mark = ","),
      "If final_active is non-zero, final-death comparisons may be censored by the simulation horizon. The projected value is only an interpretive diagnostic."
    ),
    stringsAsFactors = FALSE
  )
}

