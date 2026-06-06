# Core pure helpers extracted from app_epidemiologic_v17_academic_freeze.R
# Do not translate function names or internal identifiers.

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  dlat <- (lat2 - lat1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}


format_big <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}


scenario_multiplier <- function(x) {
  switch(
    x,
    "restricted" = 0.25,
    "reduced" = 0.60,
    "reference" = 1.00,
    "high" = 1.50,
    1.00
  )
}


variant_calibration_values <- function(x) {
  switch(
    x,
    "conservative" = list(observable_fraction = 0.001, establishment_multiplier = 0.6, label = "Conservative"),
    "reference" = list(observable_fraction = 0.005, establishment_multiplier = 1.0, label = "Reference RNA-virus macro calibration"),
    "high" = list(observable_fraction = 0.020, establishment_multiplier = 1.8, label = "High-incidence / pandemic-scale calibration"),
    list(observable_fraction = 0.005, establishment_multiplier = 1.0, label = "Reference RNA-virus macro calibration")
  )
}


calculate_metrics <- function(results, N) {
  peak_idx <- which.max(results$I)
  peak_day <- results$time[peak_idx]
  peak_infected <- results$I[peak_idx]
  final_infected <- N - min(results$S, na.rm = TRUE)
  total_deaths <- max(results$D, na.rm = TRUE)
  total_recovered <- max(results$R, na.rm = TRUE)
  list(
    peak_day = peak_day,
    peak_infected = peak_infected,
    final_infected = final_infected,
    total_deaths = total_deaths,
    total_recovered = total_recovered,
    attack_rate = (final_infected / N) * 100,
    death_rate = ifelse(final_infected > 0, (total_deaths / final_infected) * 100, 0)
  )
}


make_infectivity_kernel <- function(duration_days, profile = "mid") {
  # Discrete Gaussian-like infectiousness profile over the infectious window.
  # It is normalized to sum to 1, so R0 remains interpretable as the total
  # expected secondary infections over the full infectious period.
  duration_days <- max(1, as.integer(round(duration_days)))
  age <- seq(0, duration_days - 1)

  if (duration_days == 1) {
    kernel <- 1
  } else {
    pars <- switch(
      profile,
      "early" = list(mu = 0.30 * (duration_days - 1), sigma = max(1, 0.18 * duration_days)),
      "late" = list(mu = 0.65 * (duration_days - 1), sigma = max(1, 0.22 * duration_days)),
      "flat" = list(mu = NA_real_, sigma = NA_real_),
      list(mu = 0.45 * (duration_days - 1), sigma = max(1, 0.20 * duration_days))
    )

    if (identical(profile, "flat")) {
      kernel <- rep(1 / duration_days, duration_days)
    } else {
      kernel <- exp(-0.5 * ((age - pars$mu) / pars$sigma) ^ 2)
      kernel <- kernel / sum(kernel)
    }
  }

  names(kernel) <- paste0("age_", age)
  kernel
}


import_seed_probability <- function(rt_destination, remaining_infectivity, opportunity_modifier = 1) {
  m <- pmax(0, rt_destination) * pmax(0, remaining_infectivity)
  p <- 1 - exp(-m)
  p <- p * max(0, opportunity_modifier)
  pmax(0, pmin(1, p))
}


keep_country_names <- function(x, countries) {
  x <- as.numeric(x)
  names(x) <- countries
  x
}

