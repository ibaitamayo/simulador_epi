# Age-related pure helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

calibrate_cfr_profile_to_weighted_target <- function(raw_multiplier, population_share, target_cfr, cfr_cap = 0.95) {
  raw_multiplier <- as.numeric(raw_multiplier)
  population_share <- as.numeric(population_share)
  target_cfr <- max(0, min(as.numeric(target_cfr), cfr_cap))
  cfr_cap <- max(0, min(as.numeric(cfr_cap), 1))

  if (target_cfr <= 0 || sum(population_share) <= 0 || sum(raw_multiplier) <= 0) {
    return(rep(0, length(raw_multiplier)))
  }

  population_share <- population_share / sum(population_share)
  cfr <- rep(0, length(raw_multiplier))
  active <- rep(TRUE, length(raw_multiplier))
  remaining_target <- target_cfr

  for (iter in seq_len(50)) {
    active_weight <- sum(population_share[active] * raw_multiplier[active])
    if (active_weight <= 0) break

    scale <- remaining_target / active_weight
    tentative <- raw_multiplier[active] * scale
    over_cap <- tentative > cfr_cap

    if (!any(over_cap)) {
      cfr[active] <- tentative
      break
    }

    active_index <- which(active)
    capped_index <- active_index[over_cap]
    cfr[capped_index] <- cfr_cap
    active[capped_index] <- FALSE

    achieved_fixed <- sum(population_share[!active] * cfr[!active])
    remaining_target <- target_cfr - achieved_fixed
    if (remaining_target <= 0 || !any(active)) break
  }

  pmax(0, pmin(cfr_cap, cfr))
}


make_age_group_parameters <- function(preset = "world_average", mortality_rate = 0.01, neutral = TRUE, cfr_scale = 1) {
  age_params <- data.frame(
    age_group = c("0-9", "10-19", "20-39", "40-59", "60-79", "80+"),
    lower_age = c(0, 10, 20, 40, 60, 80),
    upper_age = c(9, 19, 39, 59, 79, Inf),
    population_share = c(0.17, 0.16, 0.31, 0.23, 0.11, 0.02),
    susceptibility_weight = rep(1, 6),
    contact_weight = rep(1, 6),
    cfr = rep(mortality_rate, 6),
    initial_distribution_weight = c(0.08, 0.12, 0.38, 0.27, 0.12, 0.03),
    cfr_profile_multiplier = rep(1, 6),
    stringsAsFactors = FALSE
  )

  age_params$population_share <- age_params$population_share / sum(age_params$population_share)
  age_params$initial_distribution_weight <- age_params$initial_distribution_weight / sum(age_params$initial_distribution_weight)

  target_cfr <- max(0, min(0.95, mortality_rate * cfr_scale))

  if (!isTRUE(neutral)) {
    raw_multiplier <- c(0.05, 0.05, 0.20, 0.75, 3.00, 8.00)
    age_params$cfr <- calibrate_cfr_profile_to_weighted_target(
      raw_multiplier = raw_multiplier,
      population_share = age_params$population_share,
      target_cfr = target_cfr,
      cfr_cap = 0.95
    )
    mean_cfr <- sum(age_params$population_share * age_params$cfr)
    if (mean_cfr > 0) {
      age_params$cfr_profile_multiplier <- age_params$cfr / mean_cfr
    } else {
      age_params$cfr_profile_multiplier <- rep(0, nrow(age_params))
    }
  } else {
    age_params$cfr <- rep(target_cfr, nrow(age_params))
    age_params$cfr_profile_multiplier <- rep(1, nrow(age_params))
  }

  age_params$target_weighted_cfr <- target_cfr
  age_params$achieved_weighted_cfr <- sum(age_params$population_share * age_params$cfr)
  age_params$cfr_saturated <- age_params$cfr >= 0.95 - 1e-12
  age_params$cfr_saturated_groups <- sum(age_params$cfr_saturated)
  age_params$min_age_cfr <- min(age_params$cfr, na.rm = TRUE)
  age_params$max_age_cfr <- max(age_params$cfr, na.rm = TRUE)
  warning_parts <- character(0)
  if (target_cfr > 0.05) {
    warning_parts <- c(warning_parts, "extreme_global_cfr_target")
  }
  if (any(age_params$cfr_saturated)) {
    warning_parts <- c(warning_parts, "one_or_more_age_groups_at_cfr_cap")
  }
  if (length(warning_parts) == 0) warning_parts <- "none"
  age_params$cfr_warning <- paste(warning_parts, collapse = ";")
  age_params
}


normalize_age_distribution_matrix <- function(mat, age_groups) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"
  if (is.null(colnames(mat))) colnames(mat) <- age_groups
  missing_cols <- setdiff(age_groups, colnames(mat))
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) mat <- cbind(mat, setNames(rep(NA_real_, nrow(mat)), cc))
  }
  mat <- mat[, age_groups, drop = FALSE]
  row_sums <- rowSums(mat, na.rm = TRUE)
  bad <- !is.finite(row_sums) | row_sums <= 0
  if (any(!bad)) mat[!bad, ] <- mat[!bad, , drop = FALSE] / row_sums[!bad]
  mat
}


extract_age_group_matrix <- function(obj, age_groups) {
  # Accept both app-native names (0-9, 10-19, ...) and generator-safe names
  # (age_0_9, age_10_19, ..., age_80_plus).
  aliases <- list(
    "0-9" = c("0-9", "age_0_9", "g0_9", "age0_9"),
    "10-19" = c("10-19", "age_10_19", "g10_19", "age10_19"),
    "20-39" = c("20-39", "age_20_39", "g20_39", "age20_39"),
    "40-59" = c("40-59", "age_40_59", "g40_59", "age40_59"),
    "60-79" = c("60-79", "age_60_79", "g60_79", "age60_79"),
    "80+" = c("80+", "80_plus", "age_80_plus", "age_80p", "g80p", "age80_plus")
  )
  out <- matrix(NA_real_, nrow = nrow(obj), ncol = length(age_groups), dimnames = list(NULL, age_groups))
  for (ag in age_groups) {
    hit <- intersect(aliases[[ag]], names(obj))
    if (length(hit) == 0) {
      stop("Age distribution file is missing column for age group ", ag,
           ". Accepted aliases include: ", paste(aliases[[ag]], collapse = ", "), call. = FALSE)
    }
    out[, ag] <- suppressWarnings(as.numeric(obj[[hit[1]]]))
  }
  out
}


model_age_share_from_country_distribution <- function(country_populations, country_age_distribution) {
  w <- as.numeric(country_populations)
  if (sum(w, na.rm = TRUE) <= 0) w <- rep(1, length(w))
  shares <- colSums(country_age_distribution * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
  shares / sum(shares)
}


update_age_params_for_model_age_share <- function(age_params, model_age_share, mortality_rate, neutral = TRUE, cfr_scale = 1) {
  model_age_share <- as.numeric(model_age_share)
  model_age_share <- model_age_share / sum(model_age_share)
  age_params$population_share <- model_age_share

  target_cfr <- max(0, min(0.95, mortality_rate * cfr_scale))
  if (!isTRUE(neutral)) {
    raw_multiplier <- c(0.05, 0.05, 0.20, 0.75, 3.00, 8.00)
    age_params$cfr <- calibrate_cfr_profile_to_weighted_target(
      raw_multiplier = raw_multiplier,
      population_share = age_params$population_share,
      target_cfr = target_cfr,
      cfr_cap = 0.95
    )
    mean_cfr <- sum(age_params$population_share * age_params$cfr)
    age_params$cfr_profile_multiplier <- if (mean_cfr > 0) age_params$cfr / mean_cfr else rep(0, nrow(age_params))
  } else {
    age_params$cfr <- rep(target_cfr, nrow(age_params))
    age_params$cfr_profile_multiplier <- rep(1, nrow(age_params))
  }

  age_params$target_weighted_cfr <- target_cfr
  age_params$achieved_weighted_cfr <- sum(age_params$population_share * age_params$cfr)
  age_params$cfr_saturated <- age_params$cfr >= 0.95 - 1e-12
  age_params$cfr_saturated_groups <- sum(age_params$cfr_saturated)
  age_params$min_age_cfr <- min(age_params$cfr, na.rm = TRUE)
  age_params$max_age_cfr <- max(age_params$cfr, na.rm = TRUE)
  warning_parts <- character(0)
  if (target_cfr > 0.05) warning_parts <- c(warning_parts, "extreme_global_cfr_target")
  if (any(age_params$cfr_saturated)) warning_parts <- c(warning_parts, "one_or_more_age_groups_at_cfr_cap")
  if (length(warning_parts) == 0) warning_parts <- "none"
  age_params$cfr_warning <- paste(warning_parts, collapse = ";")
  age_params
}


format_country_age_distribution_for_display <- function(country_age_distribution, max_rows = 50) {
  out <- as.data.frame(country_age_distribution)
  out$country <- rownames(country_age_distribution)
  out <- out[, c("country", colnames(country_age_distribution))]
  for (nm in setdiff(names(out), "country")) out[[nm]] <- sprintf("%.1f%%", 100 * as.numeric(out[[nm]]))
  head(out, max_rows)
}


format_age_parameters_for_display <- function(age_params) {
  out <- age_params[, c("age_group", "lower_age", "upper_age", "population_share", "susceptibility_weight", "contact_weight", "cfr", "cfr_profile_multiplier", "initial_distribution_weight", "target_weighted_cfr", "achieved_weighted_cfr", "cfr_saturated", "cfr_warning"), drop = FALSE]
  out$population_share <- sprintf("%.1f%%", 100 * out$population_share)
  out$susceptibility_weight <- sprintf("%.2f", out$susceptibility_weight)
  out$contact_weight <- sprintf("%.2f", out$contact_weight)
  out$cfr <- sprintf("%.3f%%", 100 * out$cfr)
  out$cfr_profile_multiplier <- sprintf("%.2f", out$cfr_profile_multiplier)
  out$initial_distribution_weight <- sprintf("%.1f%%", 100 * out$initial_distribution_weight)
  out$target_weighted_cfr <- sprintf("%.3f%%", 100 * out$target_weighted_cfr)
  out$achieved_weighted_cfr <- sprintf("%.3f%%", 100 * out$achieved_weighted_cfr)
  out$cfr_saturated <- ifelse(out$cfr_saturated, "yes", "no")
  names(out) <- c(
    "Age group", "Lower age", "Upper age", "Population share",
    "Susceptibility weight", "Contact weight", "CFR", "CFR relative multiplier",
    "Initial active share", "Target weighted CFR", "Achieved weighted CFR",
    "At CFR cap", "CFR warning"
  )
  out
}

