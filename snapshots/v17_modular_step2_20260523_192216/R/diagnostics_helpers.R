# Diagnostics-related helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

calculate_dynamic_diagnostics <- function(sim_result, base_R0 = NA_real_, max_R0_multiplier = NA_real_) {
  if (is.null(sim_result)) {
    return(data.frame(Metric = "dynamic_diagnostic_status", Value = "not_available", stringsAsFactors = FALSE))
  }

  base_R0 <- suppressWarnings(as.numeric(base_R0))
  max_R0_multiplier <- suppressWarnings(as.numeric(max_R0_multiplier))
  if (!is.finite(base_R0)) base_R0 <- NA_real_
  if (!is.finite(max_R0_multiplier)) max_R0_multiplier <- NA_real_
  requested_cap <- if (is.finite(base_R0) && is.finite(max_R0_multiplier)) base_R0 * max_R0_multiplier else NA_real_

  candidates <- sim_result$candidate_mutations
  variants <- sim_result$variants_emerged
  vf <- sim_result$variant_frequency_history
  if (is.null(candidates)) candidates <- data.frame()
  if (is.null(variants)) variants <- data.frame()
  if (is.null(vf)) vf <- data.frame()

  established <- if (nrow(variants) > 0 && "variant_id" %in% names(variants)) {
    variants[variants$variant_id != 1, , drop = FALSE]
  } else data.frame()

  candidate_R0 <- if (nrow(candidates) > 0 && "candidate_R0" %in% names(candidates)) suppressWarnings(as.numeric(candidates$candidate_R0)) else numeric()
  established_R0 <- if (nrow(established) > 0 && "R0_value" %in% names(established)) suppressWarnings(as.numeric(established$R0_value)) else numeric()
  max_candidate_R0 <- if (length(candidate_R0) > 0 && any(is.finite(candidate_R0))) max(candidate_R0, na.rm = TRUE) else NA_real_
  max_established_R0 <- if (length(established_R0) > 0 && any(is.finite(established_R0))) max(established_R0, na.rm = TRUE) else NA_real_
  mean_established_R0 <- if (length(established_R0) > 0 && any(is.finite(established_R0))) mean(established_R0, na.rm = TRUE) else NA_real_

  max_candidate_advantage <- if (is.finite(base_R0) && is.finite(max_candidate_R0)) max_candidate_R0 / base_R0 - 1 else NA_real_
  max_established_advantage <- if (is.finite(base_R0) && is.finite(max_established_R0)) max_established_R0 / base_R0 - 1 else NA_real_
  cap_tol <- if (is.finite(requested_cap)) max(1e-8, 0.001 * requested_cap) else NA_real_
  candidate_cap_binding_events <- if (is.finite(requested_cap) && length(candidate_R0) > 0) sum(candidate_R0 >= requested_cap - cap_tol, na.rm = TRUE) else NA_integer_
  established_cap_binding_events <- if (is.finite(requested_cap) && length(established_R0) > 0) sum(established_R0 >= requested_cap - cap_tol, na.rm = TRUE) else NA_integer_

  dominant_id <- NA_integer_; dominant_share <- NA_real_; dominant_R0 <- NA_real_
  if (nrow(vf) > 0 && all(c("day", "variant_id", "frequency") %in% names(vf))) {
    last_day <- max(vf$day, na.rm = TRUE)
    final_vf <- vf[vf$day == last_day, , drop = FALSE]
    if (nrow(final_vf) > 0) {
      idx <- which.max(final_vf$frequency)
      dominant_id <- suppressWarnings(as.integer(final_vf$variant_id[idx]))
      dominant_share <- suppressWarnings(as.numeric(final_vf$frequency[idx]))
      if ("R0_value" %in% names(final_vf)) dominant_R0 <- suppressWarnings(as.numeric(final_vf$R0_value[idx]))
    }
  }

  cap_usage_note <- "not_applicable"
  if (is.finite(requested_cap) && is.finite(max_candidate_R0)) {
    if (!is.na(candidate_cap_binding_events) && candidate_cap_binding_events > 0) cap_usage_note <- "cap_reached_by_candidate_events"
    else if (max_candidate_R0 < requested_cap * 0.75) cap_usage_note <- "observed_candidates_far_below_requested_cap"
    else cap_usage_note <- "cap_not_reached_but_range_used"
  }

  data.frame(
    Metric = c(
      "dynamic_max_R0_multiplier_requested", "dynamic_R0_cap_requested", "dynamic_max_candidate_R0_observed",
      "dynamic_max_candidate_advantage_observed", "dynamic_max_established_variant_R0",
      "dynamic_max_established_variant_advantage", "dynamic_mean_established_variant_R0",
      "dynamic_dominant_variant_id_final", "dynamic_dominant_variant_share_final", "dynamic_dominant_variant_R0_final",
      "dynamic_candidate_cap_binding_events", "dynamic_established_cap_binding_events", "dynamic_cap_usage_note", "dynamic_monotonicity_note"
    ),
    Value = c(
      ifelse(is.finite(max_R0_multiplier), sprintf("%.4f", max_R0_multiplier), NA),
      ifelse(is.finite(requested_cap), sprintf("%.4f", requested_cap), NA),
      ifelse(is.finite(max_candidate_R0), sprintf("%.4f", max_candidate_R0), NA),
      ifelse(is.finite(max_candidate_advantage), sprintf("%.4f", max_candidate_advantage), NA),
      ifelse(is.finite(max_established_R0), sprintf("%.4f", max_established_R0), NA),
      ifelse(is.finite(max_established_advantage), sprintf("%.4f", max_established_advantage), NA),
      ifelse(is.finite(mean_established_R0), sprintf("%.4f", mean_established_R0), NA),
      ifelse(is.na(dominant_id), NA, as.character(dominant_id)),
      ifelse(is.finite(dominant_share), sprintf("%.6f", dominant_share), NA),
      ifelse(is.finite(dominant_R0), sprintf("%.4f", dominant_R0), NA),
      ifelse(is.na(candidate_cap_binding_events), NA, as.character(candidate_cap_binding_events)),
      ifelse(is.na(established_cap_binding_events), NA, as.character(established_cap_binding_events)),
      cap_usage_note,
      "single_seed_stochastic_run; preset_pressure_does_not_guarantee_ordinal_final_R0"
    ),
    stringsAsFactors = FALSE
  )
}
