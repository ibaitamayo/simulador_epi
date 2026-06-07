# Calibration diagnostics helpers.
# Pure calculation helpers used by the Shiny server.

calculate_calibration_diagnostics_core <- function(
    d,
    calibration_population,
    observed_positive_cumulative,
    case_detection_fraction,
    observed_positive_day,
    positive_lag_days,
    observed_seroprevalence_percent,
    observed_seroprevalence_day,
    seroprevalence_lag_days,
    enable_day0_date_estimation,
    observed_positive_calendar_date,
    observed_seroprevalence_calendar_date,
    world_population) {

  if (is.null(d) || nrow(d) == 0) return(NULL)

  cumulative_sim <- d$I + d$R + d$D
  max_day <- max(d$time, na.rm = TRUE)

  get_sim_value <- function(day_value) {
    day_value <- max(0, min(max_day, as.integer(round(day_value))))
    row_idx <- which(d$time == day_value)
    if (length(row_idx) == 0) row_idx <- which.min(abs(d$time - day_value))
    list(day = d$time[row_idx[1]], value = cumulative_sim[row_idx[1]])
  }

  pop_ref <- suppressWarnings(as.numeric(calibration_population))
  if (is.na(pop_ref) || pop_ref <= 0) pop_ref <- world_population

  pos_obs <- suppressWarnings(as.numeric(observed_positive_cumulative))

  detection_fraction <- suppressWarnings(as.numeric(case_detection_fraction))
  if (is.na(detection_fraction) || detection_fraction <= 0) detection_fraction <- NA_real_
  if (!is.na(detection_fraction)) detection_fraction <- min(1, max(0.0001, detection_fraction))

  pos_day <- suppressWarnings(as.numeric(observed_positive_day))
  pos_lag <- suppressWarnings(as.numeric(positive_lag_days))
  if (is.na(pos_day)) pos_day <- NA
  if (is.na(pos_lag)) pos_lag <- 5

  sero_pct <- suppressWarnings(as.numeric(observed_seroprevalence_percent))
  sero_day <- suppressWarnings(as.numeric(observed_seroprevalence_day))
  sero_lag <- suppressWarnings(as.numeric(seroprevalence_lag_days))
  if (is.na(sero_day)) sero_day <- NA
  if (is.na(sero_lag)) sero_lag <- 26

  estimate_day0 <- isTRUE(enable_day0_date_estimation)

  parse_date_input <- function(x) {
    if (!estimate_day0 || is.null(x) || length(x) == 0 || is.na(x)) return(as.Date(NA))
    as.Date(x)
  }

  pos_calendar_date <- parse_date_input(observed_positive_calendar_date)
  sero_calendar_date <- parse_date_input(observed_seroprevalence_calendar_date)

  estimate_day0_date <- function(obs_date, obs_day) {
    if (is.na(obs_date) || is.na(obs_day)) return(as.Date(NA))
    obs_date - as.integer(round(obs_day))
  }

  estimate_event_date <- function(obs_date, lag_days) {
    if (is.na(obs_date) || is.na(lag_days)) return(as.Date(NA))
    obs_date - as.integer(round(lag_days))
  }

  pos_comparable_day <- if (!is.na(pos_day)) max(0, pos_day - pos_lag) else NA
  sero_comparable_day <- if (!is.na(sero_day)) max(0, sero_day - sero_lag) else NA

  pos_sim <- if (!is.na(pos_comparable_day)) get_sim_value(pos_comparable_day) else list(day = NA, value = NA)
  sero_sim <- if (!is.na(sero_comparable_day)) get_sim_value(sero_comparable_day) else list(day = NA, value = NA)

  sero_estimated_count <- if (!is.na(sero_pct)) pop_ref * sero_pct / 100 else NA

  safe_ratio <- function(sim, obs) {
    if (is.na(obs) || obs <= 0 || is.na(sim)) return(NA_real_)
    sim / obs
  }

  data.frame(
    target = c("observed_positives", "observed_seroprevalence"),
    observation_day = c(pos_day, sero_day),
    lag_days = c(pos_lag, sero_lag),
    comparable_simulation_day = c(pos_sim$day, sero_sim$day),
    observed_value = c(pos_obs, sero_estimated_count),
    observed_input = c(
      ifelse(is.na(pos_obs), NA, paste0(round(pos_obs), " cumulative positives")),
      ifelse(is.na(sero_pct), NA, paste0(sero_pct, "% of ", round(pop_ref)))
    ),
    simulated_cumulative = c(pos_sim$value, sero_sim$value),
    case_detection_fraction = c(detection_fraction, NA_real_),
    simulated_comparable_value = c(
      ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction),
      sero_sim$value
    ),
    simulated_to_observed_ratio = c(
      safe_ratio(ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction), pos_obs),
      safe_ratio(sero_sim$value, sero_estimated_count)
    ),
    relative_error = c(
      safe_ratio(ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction), pos_obs) - 1,
      safe_ratio(sero_sim$value, sero_estimated_count) - 1
    ),
    observation_calendar_date = as.character(c(pos_calendar_date, sero_calendar_date)),
    estimated_event_calendar_date = as.character(c(
      estimate_event_date(pos_calendar_date, pos_lag),
      estimate_event_date(sero_calendar_date, sero_lag)
    )),
    estimated_simulated_day0_date = as.character(c(
      estimate_day0_date(pos_calendar_date, pos_day),
      estimate_day0_date(sero_calendar_date, sero_day)
    )),
    stringsAsFactors = FALSE
  )
}

calculate_detection_fraction_diagnostics_core <- function(
    diag,
    enable_calibration_targets,
    case_detection_fraction,
    estimate_detection_from_serology) {

  if (!isTRUE(enable_calibration_targets)) return(NULL)
  if (is.null(diag)) return(NULL)

  pos_row <- diag[diag$target == "observed_positives", , drop = FALSE]
  sero_row <- diag[diag$target == "observed_seroprevalence", , drop = FALSE]

  user_fraction <- suppressWarnings(as.numeric(case_detection_fraction))
  if (is.na(user_fraction) || user_fraction <= 0) user_fraction <- NA_real_
  if (!is.na(user_fraction)) user_fraction <- min(1, max(0.0001, user_fraction))

  pos_obs <- if (nrow(pos_row) == 1) suppressWarnings(as.numeric(pos_row$observed_value)) else NA_real_
  sero_estimated_count <- if (nrow(sero_row) == 1) suppressWarnings(as.numeric(sero_row$observed_value)) else NA_real_

  implied_fraction <- if (
    isTRUE(estimate_detection_from_serology) &&
      !is.na(pos_obs) &&
      !is.na(sero_estimated_count) &&
      sero_estimated_count > 0
  ) {
    pos_obs / sero_estimated_count
  } else {
    NA_real_
  }

  fraction_ratio <- if (!is.na(implied_fraction) && implied_fraction > 0 && !is.na(user_fraction)) {
    user_fraction / implied_fraction
  } else {
    NA_real_
  }

  day0_pos <- if (nrow(pos_row) == 1) as.Date(pos_row$estimated_simulated_day0_date) else as.Date(NA)
  day0_sero <- if (nrow(sero_row) == 1) as.Date(sero_row$estimated_simulated_day0_date) else as.Date(NA)
  day0_diff <- if (!is.na(day0_pos) && !is.na(day0_sero)) as.integer(abs(day0_pos - day0_sero)) else NA_integer_

  warning_parts <- character(0)
  if (is.na(implied_fraction)) warning_parts <- c(warning_parts, "insufficient_inputs_for_detection_fraction")
  if (!is.na(implied_fraction) && implied_fraction > 1) warning_parts <- c(warning_parts, "implied_detection_fraction_above_1")
  if (!is.na(implied_fraction) && implied_fraction < 0.001) warning_parts <- c(warning_parts, "very_low_implied_detection_fraction")
  if (!is.na(fraction_ratio) && (fraction_ratio > 2 || fraction_ratio < 0.5)) warning_parts <- c(warning_parts, "user_detection_fraction_differs_from_serology_implied_fraction")
  if (!is.na(day0_diff) && day0_diff > 21) warning_parts <- c(warning_parts, "calendar_alignment_difference_above_21_days")
  if (length(warning_parts) == 0) warning_parts <- "none"

  data.frame(
    metric = c(
      "positive_count_unit",
      "seroprevalence_unit",
      "observed_positive_cumulative",
      "seroprevalence_estimated_cumulative",
      "user_provided_detection_fraction",
      "serology_implied_detection_fraction",
      "user_to_implied_fraction_ratio",
      "positive_based_day0_date",
      "seroprevalence_based_day0_date",
      "day0_difference_days",
      "diagnostic_warning"
    ),
    value = c(
      "absolute_count",
      "percent_of_reference_population",
      ifelse(is.na(pos_obs), NA, format(round(pos_obs), scientific = FALSE, big.mark = ",")),
      ifelse(is.na(sero_estimated_count), NA, format(round(sero_estimated_count), scientific = FALSE, big.mark = ",")),
      ifelse(is.na(user_fraction), NA, sprintf("%.8f", user_fraction)),
      ifelse(is.na(implied_fraction), NA, sprintf("%.8f", implied_fraction)),
      ifelse(is.na(fraction_ratio), NA, sprintf("%.3f", fraction_ratio)),
      as.character(day0_pos),
      as.character(day0_sero),
      ifelse(is.na(day0_diff), NA, as.character(day0_diff)),
      paste(warning_parts, collapse = ";")
    ),
    stringsAsFactors = FALSE
  )
}
