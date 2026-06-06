
# ============================================================================
# SHINY APP: MACROSCOPIC SIR MODEL WITH WORLD MAP, AIR-TRAFFIC SPREAD,
# AND STOCHASTIC VARIANT REPLACEMENT
# Version v13: infection-age kernel, Rt-dependent imported establishment,
# preserved country names after vector operations, and explicit diffusion diagnostics.
# ============================================================================

library(shiny)
library(plotly)
library(dplyr)
library(leaflet)
library(htmltools)

source("R/helpers_core.R")
source("R/age_helpers.R")

# ============================================================================
# CONSTANTS AND COUNTRY DATA
# ============================================================================

WORLD_POPULATION <- 8000000000

COUNTRIES_LIST <- c(
  "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Canada", "Chile",
  "China", "Colombia", "Denmark", "Egypt", "Finland", "France", "Germany",
  "Greece", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy",
  "Japan", "Kenya", "Malaysia", "Mexico", "Morocco", "Netherlands", "New Zealand",
  "Nigeria", "Norway", "Pakistan", "Peru", "Philippines", "Poland", "Portugal",
  "Russia", "Saudi Arabia", "Singapore", "South Africa", "South Korea", "Spain",
  "Sweden", "Switzerland", "Thailand", "Turkey", "United Arab Emirates",
  "United Kingdom", "United States", "Vietnam"
)

COUNTRY_COORDS <- data.frame(
  country = COUNTRIES_LIST,
  lng = c(-65, 133.9, 10.5, 4.5, -51.9, -106, -71, 104, -74, 9.5, 30.8, 26.1, 2.2, 10.5,
          23.7, 78, 113.9, 53.0, 44.4, -7.6, 34.8, 12.8, 138.3, 37.9, 101.9, -102, -7.1,
          5.8, 166.4, 8.7, 10.5, 69.3, -75, 121.8, 19.1, -8.2, 105, 45.0, 103.8, 22.9,
          127.5, -3.7, 18.6, 8.2, 100.5, 35.2, 54.0, -3.4, -97, 106.6),
  lat = c(-34, -25.3, 47.5, 50.8, -10, 56, -35, 35.9, 4.6, 56.2, 26.8, 61.9, 46.2, 51.2,
          39.0, 20.6, -0.8, 32.4, 33.2, 53.1, 31.0, 41.9, 36.2, -0.02, 4.2, 23.6, 31.8,
          52.1, -40.9, 9.1, 60.5, 30.4, -9.2, 12.9, 51.9, 39.4, 61.5, 23.9, 1.3, -30.6,
          35.9, 40.4, 60.1, 46.8, 15.9, 38.9, 23.4, 55.4, 38, 16.2),
  stringsAsFactors = FALSE
)


COUNTRY_METADATA <- data.frame(
  country = COUNTRIES_LIST,
  continent = c(
    "South America", "Oceania", "Europe", "Europe", "South America", "North America", "South America",
    "Asia", "South America", "Europe", "Africa", "Europe", "Europe", "Europe",
    "Europe", "Asia", "Asia", "Asia", "Asia", "Europe", "Asia", "Europe",
    "Asia", "Africa", "Asia", "North America", "Africa", "Europe", "Oceania",
    "Africa", "Europe", "Asia", "South America", "Asia", "Europe", "Europe",
    "Europe", "Asia", "Asia", "Africa", "Asia", "Europe", "Europe", "Europe",
    "Asia", "Asia", "Asia", "Europe", "North America", "Asia"
  ),
  stringsAsFactors = FALSE
)

CONTINENTS_LIST <- sort(unique(COUNTRY_METADATA$continent))

COUNTRY_TRAVEL_PARAMS <- data.frame(
  country = COUNTRIES_LIST,
  population_millions = c(
    46, 26, 9, 12, 216, 40, 20, 1410, 52, 6, 111, 6, 68, 84,
    10, 1420, 278, 89, 44, 5, 10, 59, 125, 55, 34, 129, 37,
    18, 5, 224, 5, 241, 34, 117, 38, 10, 144, 36, 6, 60,
    52, 48, 10, 9, 72, 85, 10, 67, 335, 99
  ),
  # Broad annual international passenger/trip scale. Used only for residual
  # traffic when an explicit country-pair edge is absent.
  annual_outbound_trips_millions = c(
    10, 12, 12, 18, 12, 33, 7, 120, 5, 10, 5, 8, 30, 95,
    8, 25, 12, 8, 3, 12, 8, 35, 20, 4, 12, 20, 5,
    24, 3, 2, 9, 6, 4, 8, 13, 8, 45, 20, 18, 6,
    28, 24, 18, 18, 12, 20, 22, 90, 100, 10
  ),
  stringsAsFactors = FALSE
)

# Approximate bidirectional annual passenger corridors, in millions.
# These are intended as epidemiological transport weights, not official legal statistics.
# They can be replaced by ICAO/IATA/Eurostat/BTS country-pair files when available.
PASSENGER_TRAFFIC_EDGES <- data.frame(
  origin = c(
    "Argentina",
    "Argentina",
    "Argentina",
    "Argentina",
    "Argentina",
    "Argentina",
    "Argentina",
    "Argentina",
    "Australia",
    "Australia",
    "Australia",
    "Australia",
    "Australia",
    "Australia",
    "Australia",
    "Australia",
    "Austria",
    "Austria",
    "Austria",
    "Austria",
    "Austria",
    "Belgium",
    "Belgium",
    "Belgium",
    "Belgium",
    "Belgium",
    "Belgium",
    "Brazil",
    "Brazil",
    "Brazil",
    "Brazil",
    "Brazil",
    "Brazil",
    "Brazil",
    "Brazil",
    "Canada",
    "Canada",
    "Canada",
    "Canada",
    "Canada",
    "Canada",
    "Canada",
    "Chile",
    "Chile",
    "Chile",
    "Chile",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "China",
    "Colombia",
    "Colombia",
    "Colombia",
    "Colombia",
    "Denmark",
    "Denmark",
    "Denmark",
    "Denmark",
    "Denmark",
    "Egypt",
    "Egypt",
    "Egypt",
    "Egypt",
    "Egypt",
    "Egypt",
    "Egypt",
    "Finland",
    "Finland",
    "Finland",
    "Finland",
    "France",
    "France",
    "France",
    "France",
    "France",
    "France",
    "France",
    "France",
    "France",
    "France",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Germany",
    "Greece",
    "Greece",
    "Greece",
    "Greece",
    "India",
    "India",
    "India",
    "India",
    "India",
    "India",
    "India",
    "India",
    "India",
    "India",
    "Indonesia",
    "Indonesia",
    "Indonesia",
    "Indonesia",
    "Indonesia",
    "Indonesia",
    "Iran",
    "Iran",
    "Iran",
    "Iran",
    "Iraq",
    "Iraq",
    "Ireland",
    "Ireland",
    "Ireland",
    "Ireland",
    "Israel",
    "Israel",
    "Israel",
    "Israel",
    "Italy",
    "Italy",
    "Italy",
    "Italy",
    "Italy",
    "Italy",
    "Italy",
    "Italy",
    "Japan",
    "Japan",
    "Japan",
    "Japan",
    "Japan",
    "Kenya",
    "Kenya",
    "Kenya",
    "Kenya",
    "Malaysia",
    "Malaysia",
    "Malaysia",
    "Malaysia",
    "Malaysia",
    "Mexico",
    "Mexico",
    "Mexico",
    "Mexico",
    "Mexico",
    "Morocco",
    "Morocco",
    "Morocco",
    "Morocco",
    "Netherlands",
    "Netherlands",
    "Netherlands",
    "Netherlands",
    "Netherlands",
    "Netherlands",
    "New Zealand",
    "New Zealand",
    "Nigeria",
    "Nigeria",
    "Nigeria",
    "Norway",
    "Norway",
    "Norway",
    "Pakistan",
    "Pakistan",
    "Pakistan",
    "Peru",
    "Peru",
    "Peru",
    "Philippines",
    "Philippines",
    "Philippines",
    "Poland",
    "Poland",
    "Poland",
    "Portugal",
    "Portugal",
    "Russia",
    "Russia",
    "Russia",
    "Russia",
    "Saudi Arabia",
    "Saudi Arabia",
    "Saudi Arabia",
    "Saudi Arabia",
    "Singapore",
    "Singapore",
    "Singapore",
    "Singapore",
    "South Africa",
    "South Africa",
    "South Africa",
    "South Korea",
    "South Korea",
    "South Korea",
    "Spain",
    "Spain",
    "Spain",
    "Spain",
    "Spain",
    "Sweden",
    "Sweden",
    "Sweden",
    "Switzerland",
    "Switzerland",
    "Thailand",
    "Thailand",
    "Turkey",
    "Turkey",
    "United Arab Emirates",
    "United Arab Emirates",
    "United Kingdom",
    "United Kingdom",
    "United States"
  ),
  dest = c(
    "Brazil",
    "Chile",
    "Peru",
    "Colombia",
    "Mexico",
    "United States",
    "Spain",
    "Italy",
    "New Zealand",
    "Singapore",
    "China",
    "Japan",
    "United States",
    "United Kingdom",
    "Indonesia",
    "Thailand",
    "Germany",
    "Italy",
    "Spain",
    "Switzerland",
    "Turkey",
    "France",
    "Germany",
    "Netherlands",
    "Spain",
    "United Kingdom",
    "Italy",
    "Chile",
    "Peru",
    "Colombia",
    "Portugal",
    "Spain",
    "United States",
    "France",
    "Italy",
    "United States",
    "United Kingdom",
    "France",
    "Germany",
    "Mexico",
    "China",
    "Japan",
    "Peru",
    "Colombia",
    "United States",
    "Spain",
    "Japan",
    "South Korea",
    "Thailand",
    "Singapore",
    "Malaysia",
    "India",
    "United States",
    "Australia",
    "Russia",
    "Germany",
    "United Kingdom",
    "Vietnam",
    "Peru",
    "Mexico",
    "United States",
    "Spain",
    "Germany",
    "Norway",
    "Sweden",
    "United Kingdom",
    "Spain",
    "Saudi Arabia",
    "United Arab Emirates",
    "Turkey",
    "Germany",
    "United Kingdom",
    "Italy",
    "France",
    "Sweden",
    "Norway",
    "Germany",
    "United Kingdom",
    "United Kingdom",
    "Spain",
    "Italy",
    "Germany",
    "Switzerland",
    "Netherlands",
    "United States",
    "Morocco",
    "Turkey",
    "Canada",
    "Spain",
    "Italy",
    "Turkey",
    "United Kingdom",
    "Netherlands",
    "Switzerland",
    "Greece",
    "United States",
    "Russia",
    "Poland",
    "United Kingdom",
    "Italy",
    "Turkey",
    "France",
    "United Arab Emirates",
    "Saudi Arabia",
    "Singapore",
    "United Kingdom",
    "United States",
    "Malaysia",
    "Thailand",
    "Canada",
    "Australia",
    "Pakistan",
    "Malaysia",
    "Singapore",
    "Thailand",
    "Australia",
    "China",
    "Saudi Arabia",
    "Turkey",
    "United Arab Emirates",
    "Iraq",
    "Germany",
    "Turkey",
    "Iran",
    "United Kingdom",
    "United States",
    "Spain",
    "France",
    "Turkey",
    "United States",
    "France",
    "United Kingdom",
    "Spain",
    "Germany",
    "France",
    "United Kingdom",
    "United States",
    "Switzerland",
    "Netherlands",
    "Greece",
    "South Korea",
    "China",
    "United States",
    "Thailand",
    "Singapore",
    "United Kingdom",
    "South Africa",
    "India",
    "United Arab Emirates",
    "Singapore",
    "Thailand",
    "China",
    "India",
    "Australia",
    "United States",
    "Canada",
    "Spain",
    "Colombia",
    "Brazil",
    "Spain",
    "France",
    "Netherlands",
    "Belgium",
    "United Kingdom",
    "Spain",
    "Germany",
    "France",
    "Italy",
    "United States",
    "Australia",
    "United States",
    "United Kingdom",
    "South Africa",
    "United Arab Emirates",
    "Sweden",
    "Denmark",
    "United Kingdom",
    "Saudi Arabia",
    "United Arab Emirates",
    "United Kingdom",
    "Chile",
    "Colombia",
    "Spain",
    "United States",
    "Saudi Arabia",
    "Singapore",
    "Germany",
    "United Kingdom",
    "Spain",
    "Spain",
    "France",
    "Turkey",
    "United Arab Emirates",
    "China",
    "Germany",
    "India",
    "Egypt",
    "Turkey",
    "United Arab Emirates",
    "Malaysia",
    "Australia",
    "India",
    "Thailand",
    "United Kingdom",
    "United Arab Emirates",
    "Germany",
    "Japan",
    "China",
    "United States",
    "United Kingdom",
    "Germany",
    "France",
    "Italy",
    "Portugal",
    "Germany",
    "Denmark",
    "Norway",
    "Germany",
    "France",
    "China",
    "Malaysia",
    "Germany",
    "Russia",
    "India",
    "United Kingdom",
    "United States",
    "Spain",
    "Mexico"
  ),
  annual_passengers_millions = c(
    4.0,
    3.0,
    1.2,
    0.8,
    0.6,
    1.3,
    1.1,
    0.4,
    6.0,
    5.5,
    4.0,
    3.0,
    4.5,
    2.5,
    3.5,
    2.5,
    6.5,
    2.5,
    2.0,
    2.8,
    1.8,
    3.5,
    3.0,
    4.5,
    2.4,
    2.2,
    1.8,
    2.0,
    1.2,
    1.0,
    1.4,
    1.8,
    3.0,
    1.0,
    0.9,
    25.0,
    5.0,
    3.0,
    3.0,
    7.0,
    2.5,
    1.8,
    1.0,
    0.6,
    1.1,
    0.7,
    10.0,
    9.0,
    11.0,
    5.5,
    5.0,
    4.0,
    5.0,
    4.0,
    2.5,
    3.0,
    3.0,
    6.0,
    1.0,
    2.5,
    2.5,
    1.2,
    2.5,
    2.0,
    2.2,
    2.0,
    2.0,
    5.0,
    4.0,
    3.0,
    2.5,
    2.0,
    1.8,
    1.5,
    1.5,
    1.2,
    1.6,
    1.4,
    18.0,
    14.0,
    8.0,
    7.0,
    4.0,
    4.0,
    5.0,
    3.0,
    3.0,
    2.2,
    13.0,
    8.0,
    8.0,
    6.5,
    5.0,
    5.0,
    3.0,
    4.5,
    2.5,
    3.0,
    3.0,
    2.8,
    2.0,
    2.0,
    11.0,
    7.0,
    5.0,
    4.5,
    4.0,
    3.0,
    2.5,
    2.0,
    1.8,
    3.0,
    5.0,
    4.0,
    3.0,
    2.0,
    2.0,
    1.5,
    3.0,
    1.2,
    1.0,
    1.0,
    1.5,
    1.0,
    12.0,
    2.2,
    1.8,
    1.2,
    1.8,
    1.5,
    1.0,
    1.0,
    7.0,
    6.0,
    5.0,
    4.5,
    3.0,
    2.5,
    2.5,
    2.0,
    5.0,
    5.0,
    5.0,
    3.0,
    2.0,
    2.0,
    1.5,
    1.0,
    1.0,
    6.0,
    5.0,
    3.0,
    2.0,
    1.8,
    32.0,
    25.0,
    1.8,
    1.4,
    1.0,
    4.0,
    3.5,
    2.0,
    1.2,
    7.0,
    5.0,
    5.0,
    4.0,
    3.0,
    3.5,
    3.5,
    1.4,
    2.0,
    1.0,
    1.5,
    1.3,
    1.5,
    1.0,
    2.5,
    2.5,
    1.8,
    1.5,
    1.0,
    0.8,
    3.0,
    2.0,
    1.5,
    2.2,
    2.0,
    1.5,
    2.5,
    2.0,
    4.0,
    3.5,
    2.5,
    3.0,
    7.0,
    3.0,
    2.5,
    6.0,
    6.0,
    4.0,
    3.0,
    2.0,
    2.0,
    2.0,
    1.5,
    3.0,
    2.0,
    2.0,
    20.0,
    13.0,
    10.0,
    8.0,
    6.0,
    3.0,
    2.5,
    2.0,
    2.2,
    2.0,
    11.0,
    5.0,
    7.0,
    6.0,
    8.0,
    5.0,
    20.0,
    20.0,
    32.0
  ),
  stringsAsFactors = FALSE
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

make_bilateral_passenger_matrix <- function(countries,
                                            air_travel_scenario = "reference",
                                            rng_seed = NULL,
                                            route_noise_sdlog = 0.25,
                                            residual_fraction = 0.05) {
  n <- length(countries)
  m <- matrix(0, nrow = n, ncol = n, dimnames = list(countries, countries))

  edges <- PASSENGER_TRAFFIC_EDGES %>%
    filter(origin %in% countries, dest %in% countries)

  if (nrow(edges) > 0) {
    for (k in seq_len(nrow(edges))) {
      a <- edges$origin[k]
      b <- edges$dest[k]
      annual_total <- edges$annual_passengers_millions[k] * 1e6
      # The edge is bidirectional annual passenger volume. Split evenly by direction.
      m[a, b] <- m[a, b] + annual_total / 2
      m[b, a] <- m[b, a] + annual_total / 2
    }
  }

  # Residual low-intensity traffic for unlisted connections, distributed by a gravity rule.
  params <- COUNTRY_TRAVEL_PARAMS[match(countries, COUNTRY_TRAVEL_PARAMS$country), ]
  coords <- COUNTRY_COORDS[match(countries, COUNTRY_COORDS$country), ]
  residual_outbound <- params$annual_outbound_trips_millions * 1e6 * residual_fraction

  for (i in seq_along(countries)) {
    weights <- rep(0, n)
    for (j in seq_along(countries)) {
      if (i != j) {
        d <- haversine_km(coords$lng[i], coords$lat[i], coords$lng[j], coords$lat[j])
        attractiveness <- sqrt(pmax(params$population_millions[j], 1)) + 0.4 * pmax(params$annual_outbound_trips_millions[j], 0.1)
        weights[j] <- attractiveness / ((1 + d / 1500) ^ 1.4)
      }
    }
    if (sum(weights, na.rm = TRUE) > 0) {
      weights <- weights / sum(weights, na.rm = TRUE)
      m[i, ] <- m[i, ] + residual_outbound[i] * weights
    }
  }

  if (!is.null(rng_seed) && is.finite(rng_seed) && route_noise_sdlog > 0) {
    set.seed(as.integer(rng_seed) + 99001)
    noise <- matrix(
      rlnorm(n * n, meanlog = 0, sdlog = route_noise_sdlog),
      nrow = n,
      ncol = n,
      dimnames = list(countries, countries)
    )
    diag(noise) <- 0
    m <- m * noise
  }

  diag(m) <- 0
  m <- m * scenario_multiplier(air_travel_scenario)
  m[!is.finite(m)] <- 0
  pmax(m, 0)
}

# Probability that an imported infectious traveller produces a local seed in a
# destination country. This is a population-level approximation, not an
# individual clinical model. It depends on destination Rt and on the fraction of
# infectiousness remaining for the traveller's infection age.
# Calibrate an age-specific CFR profile to a global weighted target.
# This is a scenario-level population parameterisation. It preserves the
# relative age gradient while matching the requested weighted mean whenever
# possible under the specified upper bound.
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

# Age-group parameters for the age-adjusted model structure.
# Step 2 keeps contact and susceptibility weights neutral. Differential CFR
# mode calibrates the age profile so that the weighted average approximates the
# global CFR input multiplied by the CFR scale factor.
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




AGE_DISTRIBUTION_RDS_FILE <- "country_age_distribution_wpp2024_6groups.rds"
AGE_DISTRIBUTION_CSV_FILE <- "country_age_distribution_wpp2024_6groups.csv"
WORLD_COUNTRY_POLYGONS_RDS_FILE <- "world_countries_simplified.rds"

# Robust lookup for external age-distribution files.
# Shiny can be launched from a working directory that is not the app file directory,
# especially when using runApp("path/to/app.R") or IDE launch buttons. Therefore the
# app searches several plausible locations instead of only getwd().
detect_current_app_dir <- function() {
  frame_paths <- tryCatch({
    frames <- sys.frames()
    vals <- unlist(lapply(frames, function(fr) {
      if (!is.null(fr$ofile)) as.character(fr$ofile) else NA_character_
    }))
    vals[!is.na(vals) & nzchar(vals)]
  }, error = function(e) character(0))

  cmd_file <- tryCatch({
    args <- commandArgs(FALSE)
    f <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(f) > 0) f[1] else character(0)
  }, error = function(e) character(0))

  candidate_files <- c(frame_paths, cmd_file)
  candidate_files <- candidate_files[file.exists(candidate_files)]
  if (length(candidate_files) > 0) {
    return(dirname(normalizePath(candidate_files[1], winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

APP_DIR <- detect_current_app_dir()

age_distribution_search_dirs <- function() {
  env_dir <- Sys.getenv("EPIDEM_AGE_DISTRIBUTION_DIR", unset = "")
  dirs <- c(env_dir, APP_DIR, getwd(), dirname(normalizePath(AGE_DISTRIBUTION_CSV_FILE, winslash = "/", mustWork = FALSE)))
  dirs <- dirs[nzchar(dirs)]
  dirs <- unique(normalizePath(dirs, winslash = "/", mustWork = FALSE))
  dirs
}

find_age_distribution_file <- function(filename) {
  # First try the explicit search directories.
  dirs <- age_distribution_search_dirs()
  candidate_paths <- file.path(dirs, filename)
  candidate_paths <- unique(normalizePath(candidate_paths, winslash = "/", mustWork = FALSE))
  hit <- candidate_paths[file.exists(candidate_paths)]
  if (length(hit) > 0) return(hit[1])

  # Then try a shallow recursive search under getwd() and the app directory.
  # This protects against RStudio/runApp using a different working directory
  # or the files being placed one folder below the launched app path.
  recursive_roots <- unique(normalizePath(c(getwd(), APP_DIR), winslash = "/", mustWork = FALSE))
  recursive_roots <- recursive_roots[dir.exists(recursive_roots)]
  for (root in recursive_roots) {
    found <- tryCatch(
      list.files(root, pattern = paste0("^", gsub("([\\.\\+\\*\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|])", "\\\\\\1", filename), "$"),
                 recursive = TRUE, full.names = TRUE, ignore.case = FALSE),
      error = function(e) character(0)
    )
    if (length(found) > 0) return(normalizePath(found[1], winslash = "/", mustWork = FALSE))
  }

  NA_character_
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

load_external_country_age_distribution <- function(countries, age_groups) {
  # The app first looks for an RDS produced by generate_country_age_distribution_wpp2024_6groups.R.
  # If absent, it tries the companion CSV. If neither exists, the internal rounded fallback is used.
  rds_path <- find_age_distribution_file(AGE_DISTRIBUTION_RDS_FILE)
  csv_path <- find_age_distribution_file(AGE_DISTRIBUTION_CSV_FILE)

  if (!is.na(rds_path) && file.exists(rds_path)) {
    obj <- readRDS(rds_path)
    if (is.data.frame(obj)) {
      country_col <- intersect(c("country", "Country", "location", "Location"), names(obj))[1]
      if (is.na(country_col)) stop("Age distribution RDS data.frame must contain a country column.")
      rn <- as.character(obj[[country_col]])
      mat <- extract_age_group_matrix(obj, age_groups)
      rownames(mat) <- rn
    } else {
      mat <- as.matrix(obj)
    }
    mat <- normalize_age_distribution_matrix(mat, age_groups)
    attr(mat, "source") <- "UN_WPP_2024_country_age_distribution_6groups_rds"
    attr(mat, "file") <- rds_path
    attr(mat, "searched_dirs") <- paste(age_distribution_search_dirs(), collapse = ";")
    return(mat)
  }

  if (!is.na(csv_path) && file.exists(csv_path)) {
    obj <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
    country_col <- intersect(c("country", "Country", "location", "Location"), names(obj))[1]
    if (is.na(country_col)) stop("Age distribution CSV must contain a country column.")
    rn <- as.character(obj[[country_col]])
    mat <- extract_age_group_matrix(obj, age_groups)
    rownames(mat) <- rn
    mat <- normalize_age_distribution_matrix(mat, age_groups)
    attr(mat, "source") <- "UN_WPP_2024_country_age_distribution_6groups_csv"
    attr(mat, "file") <- csv_path
    attr(mat, "searched_dirs") <- paste(age_distribution_search_dirs(), collapse = ";")
    return(mat)
  }

  out <- NULL
  attr(out, "searched_dirs") <- paste(age_distribution_search_dirs(), collapse = ";")
  NULL
}

# Country-specific age distributions for the six age groups used by the model.
# Values are approximate demographic shares for teaching/planning simulation.
# They are intended to replace the previous single global age pyramid and should
# be traceable/replaced by UN WPP / World Bank age-structure tables in future updates.
make_country_age_distribution <- function(countries, mode = "country_specific") {
  age_groups <- c("0-9", "10-19", "20-39", "40-59", "60-79", "80+")
  global_average <- c(0.17, 0.16, 0.31, 0.23, 0.11, 0.02)
  names(global_average) <- age_groups

  external_mat <- NULL
  if (identical(mode, "country_specific")) {
    external_mat <- tryCatch(
      load_external_country_age_distribution(countries, age_groups),
      error = function(e) {
        warning("Could not load external age distribution file: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(external_mat)) {
      out <- matrix(NA_real_, nrow = length(countries), ncol = length(age_groups), dimnames = list(countries, age_groups))
      for (cc in countries) {
        if (cc %in% rownames(external_mat)) out[cc, ] <- external_mat[cc, ] else out[cc, ] <- global_average
      }
      out <- normalize_age_distribution_matrix(out, age_groups)
      attr(out, "source") <- attr(external_mat, "source")
      attr(out, "file") <- attr(external_mat, "file")
      attr(out, "searched_dirs") <- attr(external_mat, "searched_dirs")
      return(out)
    }
  }

  normalize_row <- function(x) {
    x <- as.numeric(x)
    if (sum(x, na.rm = TRUE) <= 0) return(global_average)
    x / sum(x, na.rm = TRUE)
  }

  # Approximate country-specific shares. Columns: 0-9, 10-19, 20-39, 40-59, 60-79, 80+.
  # These values are deliberately rounded to keep the model transparent.
  tbl <- data.frame(
    country = c(
      "Argentina","Australia","Austria","Belgium","Brazil","Canada","Chile","China","Colombia","Denmark",
      "Egypt","Finland","France","Germany","Greece","India","Indonesia","Iran","Iraq","Ireland",
      "Israel","Italy","Japan","Kenya","Malaysia","Mexico","Morocco","Netherlands","New Zealand","Nigeria",
      "Norway","Pakistan","Peru","Philippines","Poland","Portugal","Russia","Saudi Arabia","Singapore","South Africa",
      "South Korea","Spain","Sweden","Switzerland","Thailand","Turkey","United Arab Emirates","United Kingdom","United States","Vietnam"
    ),
    g0_9   = c(0.14,0.12,0.09,0.11,0.13,0.10,0.12,0.11,0.15,0.11, 0.22,0.10,0.11,0.10,0.09,0.20,0.17,0.13,0.18,0.13, 0.18,0.08,0.08,0.28,0.16,0.16,0.18,0.10,0.12,0.29, 0.11,0.27,0.16,0.20,0.09,0.09,0.11,0.15,0.09,0.20, 0.07,0.09,0.11,0.10,0.11,0.13,0.10,0.11,0.12,0.15),
    g10_19 = c(0.15,0.12,0.10,0.11,0.14,0.11,0.13,0.11,0.16,0.11, 0.18,0.11,0.12,0.10,0.10,0.17,0.16,0.12,0.17,0.12, 0.16,0.09,0.09,0.25,0.15,0.16,0.17,0.11,0.13,0.24, 0.12,0.23,0.16,0.19,0.10,0.10,0.10,0.13,0.10,0.17, 0.09,0.10,0.11,0.11,0.12,0.13,0.09,0.12,0.13,0.14),
    g20_39 = c(0.30,0.27,0.24,0.25,0.31,0.27,0.30,0.27,0.32,0.25, 0.34,0.24,0.24,0.24,0.24,0.34,0.33,0.36,0.36,0.29, 0.30,0.22,0.22,0.29,0.35,0.33,0.31,0.25,0.27,0.29, 0.26,0.30,0.33,0.33,0.25,0.23,0.26,0.38,0.33,0.31, 0.24,0.23,0.25,0.25,0.31,0.31,0.50,0.25,0.27,0.32),
    g40_59 = c(0.25,0.26,0.29,0.27,0.26,0.27,0.27,0.31,0.24,0.27, 0.18,0.27,0.26,0.28,0.28,0.19,0.22,0.25,0.20,0.26, 0.22,0.29,0.27,0.11,0.24,0.24,0.21,0.27,0.26,0.11, 0.27,0.13,0.23,0.20,0.29,0.28,0.29,0.22,0.28,0.20, 0.32,0.28,0.26,0.27,0.27,0.25,0.26,0.26,0.25,0.25),
    g60_79 = c(0.14,0.18,0.28,0.25,0.14,0.20,0.15,0.17,0.11,0.25, 0.07,0.29,0.23,0.25,0.25,0.08,0.10,0.12,0.08,0.16, 0.12,0.27,0.27,0.06,0.09,0.09,0.11,0.25,0.17,0.06, 0.24,0.06,0.10,0.07,0.28,0.27,0.22,0.10,0.17,0.10, 0.30,0.26,0.24,0.24,0.16,0.15,0.05,0.21,0.19,0.12),
    g80p   = c(0.02,0.05,0.10,0.11,0.02,0.05,0.03,0.03,0.02,0.11, 0.01,0.09,0.04,0.03,0.04,0.02,0.02,0.02,0.01,0.04, 0.02,0.05,0.07,0.01,0.01,0.02,0.02,0.02,0.05,0.01, 0.00,0.01,0.02,0.01,0.03,0.03,0.02,0.02,0.03,0.02, 0.08,0.04,0.03,0.03,0.03,0.03,0.00,0.05,0.04,0.02),
    stringsAsFactors = FALSE
  )

  # Ensure every row sums to one after rounding.
  mat <- as.matrix(tbl[, c("g0_9", "g10_19", "g20_39", "g40_59", "g60_79", "g80p")])
  mat <- t(apply(mat, 1, normalize_row))
  colnames(mat) <- age_groups
  rownames(mat) <- tbl$country

  if (!identical(mode, "country_specific")) {
    out <- matrix(rep(global_average, each = length(countries)), nrow = length(countries), byrow = FALSE)
    colnames(out) <- age_groups
    rownames(out) <- countries
    attr(out, "source") <- "single_global_average_validation_profile"
    attr(out, "file") <- NA_character_
    return(out)
  }

  out <- matrix(NA_real_, nrow = length(countries), ncol = length(age_groups), dimnames = list(countries, age_groups))
  for (cc in countries) {
    if (cc %in% rownames(mat)) out[cc, ] <- mat[cc, ] else out[cc, ] <- global_average
  }
  out <- t(apply(out, 1, normalize_row))
  colnames(out) <- age_groups
  rownames(out) <- countries
  attr(out, "source") <- "internal_rounded_country_specific_fallback_table_WPP_file_not_found"
  attr(out, "file") <- NA_character_
  out
}

fix_country_polygons_for_leaflet <- function(obj) {
  if (!requireNamespace("sf", quietly = TRUE) || is.null(obj) || !inherits(obj, "sf")) return(obj)
  out <- tryCatch(sf::st_transform(obj, 4326), error = function(e) obj)
  out <- tryCatch(sf::st_make_valid(out), error = function(e) out)
  out <- tryCatch(
    suppressWarnings(sf::st_wrap_dateline(out, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)),
    error = function(e) out
  )
  out <- tryCatch(sf::st_make_valid(out), error = function(e) out)
  out
}

load_world_country_polygons <- function() {
  polygon_path <- find_age_distribution_file(WORLD_COUNTRY_POLYGONS_RDS_FILE)
  out <- list(data = NULL, file = NA_character_, source = "country_polygon_file_not_found", warning = "Country polygon file not found; using marker-only map.")
  if (is.na(polygon_path) || !file.exists(polygon_path)) return(out)

  if (!requireNamespace("sf", quietly = TRUE)) {
    out$file <- polygon_path
    out$source <- "country_polygon_file_found_but_sf_not_available"
    out$warning <- "Country polygon file found, but package sf is not available; using marker-only map."
    return(out)
  }

  obj <- tryCatch(readRDS(polygon_path), error = function(e) NULL)
  if (is.null(obj) || !inherits(obj, "sf")) {
    out$file <- polygon_path
    out$source <- "country_polygon_file_invalid"
    out$warning <- "Country polygon file could not be read as an sf object; using marker-only map."
    return(out)
  }

  if (!"sim_country" %in% names(obj)) {
    name_col <- intersect(c("name", "admin", "sovereignt", "country", "Country", "NAME", "ADMIN"), names(obj))[1]
    if (!is.na(name_col)) {
      obj$sim_country <- as.character(obj[[name_col]])
    } else {
      out$file <- polygon_path
      out$source <- "country_polygon_file_missing_sim_country"
      out$warning <- "Country polygon file lacks sim_country/name column; using marker-only map."
      return(out)
    }
  }

  obj$sim_country <- as.character(obj$sim_country)
  obj <- obj[obj$sim_country %in% COUNTRIES_LIST, , drop = FALSE]
  obj <- fix_country_polygons_for_leaflet(obj)
  if (nrow(obj) == 0) {
    out$file <- polygon_path
    out$source <- "country_polygon_file_no_matching_countries"
    out$warning <- "Country polygon file has no matching simulator countries; using marker-only map."
    return(out)
  }

  list(
    data = obj,
    file = polygon_path,
    source = "world_countries_simplified_rds_dateline_checked",
    warning = NA_character_
  )
}

WORLD_COUNTRY_POLYGONS <- load_world_country_polygons()

world_polygon_available <- function() {
  !is.null(WORLD_COUNTRY_POLYGONS$data) && inherits(WORLD_COUNTRY_POLYGONS$data, "sf")
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



# Containment presets used as scenario inputs for the active simulation only.
# These are deliberately broad teaching/planning presets, not universal constants.
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

make_containment_schedule <- function(enabled = FALSE,
                                      preset = "none",
                                      start_day = 30,
                                      end_day = 120,
                                      custom_transmission_reduction = 50,
                                      custom_mobility_reduction = 50,
                                      label = "Containment period",
                                      geographic_scope = "global",
                                      affected_continents = character(0),
                                      affected_countries = character(0),
                                      all_countries = COUNTRIES_LIST) {
  preset_table <- make_containment_preset_table()
  preset <- ifelse(preset %in% preset_table$preset, preset, "none")
  row <- preset_table[preset_table$preset == preset, , drop = FALSE]

  geographic_scope <- ifelse(geographic_scope %in% c("global", "continent", "countries"), geographic_scope, "global")
  affected_continents <- affected_continents[affected_continents %in% CONTINENTS_LIST]
  affected_countries <- affected_countries[affected_countries %in% all_countries]

  # Scope-specific resolution for calculation and reporting.
  # In global mode, ignore residual selections from hidden continent/country widgets.
  resolved_countries <- all_countries
  report_continents <- if (identical(geographic_scope, "global")) "ALL" else affected_continents
  if (identical(geographic_scope, "continent")) {
    resolved_countries <- COUNTRY_METADATA$country[COUNTRY_METADATA$continent %in% affected_continents]
    resolved_countries <- resolved_countries[resolved_countries %in% all_countries]
  } else if (identical(geographic_scope, "countries")) {
    resolved_countries <- affected_countries
    report_continents <- if (length(resolved_countries) > 0) {
      unique(COUNTRY_METADATA$continent[COUNTRY_METADATA$country %in% resolved_countries])
    } else {
      character(0)
    }
  }

  if (!isTRUE(enabled) || identical(preset, "none") || length(resolved_countries) == 0) {
    transmission_reduction <- 0
    mobility_reduction <- 0
    start_day <- NA_integer_
    end_day <- NA_integer_
    active_days <- 0
  } else if (identical(preset, "custom")) {
    transmission_reduction <- custom_transmission_reduction
    mobility_reduction <- custom_mobility_reduction
    start_day <- as.integer(round(start_day))
    end_day <- as.integer(round(end_day))
    if (end_day < start_day) {
      tmp <- start_day
      start_day <- end_day
      end_day <- tmp
    }
    active_days <- max(0, end_day - start_day + 1)
  } else {
    transmission_reduction <- row$transmission_reduction_percent[1]
    mobility_reduction <- row$mobility_reduction_percent[1]
    start_day <- as.integer(round(start_day))
    end_day <- as.integer(round(end_day))
    if (end_day < start_day) {
      tmp <- start_day
      start_day <- end_day
      end_day <- tmp
    }
    active_days <- max(0, end_day - start_day + 1)
  }

  transmission_reduction <- max(0, min(100, as.numeric(transmission_reduction)))
  mobility_reduction <- max(0, min(100, as.numeric(mobility_reduction)))
  transmission_multiplier <- 1 - transmission_reduction / 100
  mobility_multiplier <- 1 - mobility_reduction / 100

  list(
    enabled = isTRUE(enabled) && !identical(preset, "none") && active_days > 0 && length(resolved_countries) > 0,
    preset = preset,
    preset_label = row$label[1],
    label = label,
    start_day = start_day,
    end_day = end_day,
    active_days = active_days,
    transmission_reduction_percent = transmission_reduction,
    mobility_reduction_percent = mobility_reduction,
    transmission_multiplier = transmission_multiplier,
    mobility_multiplier = mobility_multiplier,
    definition = row$definition[1],
    geographic_scope = geographic_scope,
    affected_continents = report_continents,
    affected_countries = resolved_countries,
    affected_country_count = length(resolved_countries),
    mobility_application_rule = "origin_or_destination_affected",
    applies_to = "active_user_scenario_only",
    fixed_reference_modified = FALSE
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

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Arial, sans-serif; }
      .shiny-output-error { color: red; }
      h2 { color: #2C3E50; border-bottom: 2px solid #3498DB; padding-bottom: 10px; }
      h4 { color: #34495E; }
      .small-note { font-size: 12px; color: #6C7A89; }
      .assumption-box { background: #F8F9F9; padding: 8px; border-left: 4px solid #3498DB; margin-bottom: 8px; }
      .mode-box { background: #F4F6F7; padding: 10px; border-left: 4px solid #7D3C98; margin-bottom: 10px; }
      .fixed-ref-box { background: #F0F9F4; padding: 10px; border-left: 4px solid #27AE60; margin-bottom: 10px; overflow: hidden; }
      .fixed-ref-card { background: #FFFFFF; border: 1px solid #D5F5E3; border-radius: 6px; padding: 8px 10px; margin-top: 8px; }
      .fixed-ref-grid { display: grid; grid-template-columns: minmax(110px, 40%) 1fr; gap: 6px 10px; align-items: start; }
      .fixed-ref-label { font-weight: 600; color: #1E8449; }
      .fixed-ref-value { color: #2C3E50; overflow-wrap: anywhere; word-break: break-word; }
      .logic-box { border: 1px solid #D6EAF8; background: #F8FBFF; border-radius: 8px; padding: 12px; margin-bottom: 10px; }
      .logic-box.guided { border-left: 5px solid #2E86C1; }
      .logic-box.advanced { border-left: 5px solid #8E44AD; }
      .logic-box.both { border-left: 5px solid #16A085; }
      .logic-arrow { text-align: center; font-size: 24px; color: #7F8C8D; margin: -2px 0 8px 0; }
      .logic-chip { display: inline-block; padding: 3px 8px; border-radius: 12px; font-size: 12px; margin: 2px 4px 2px 0; }
      .logic-chip.guided { background: #D6EAF8; color: #1B4F72; }
      .logic-chip.advanced { background: #E8DAEF; color: #6C3483; }
      .logic-chip.both { background: #D1F2EB; color: #0E6251; }
      .simulation-action-box { position: sticky; top: 8px; z-index: 999; background: #FFF5F5; border: 1px solid #F5B7B1; border-left: 5px solid #E74C3C; border-radius: 8px; padding: 12px; margin: 12px 0 10px 0; }
      .primary-run-button .btn { width: 100%; font-size: 17px; font-weight: 700; padding: 10px 12px; }
      .secondary-action-row { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 8px; }
      .secondary-action-row .btn { width: 100%; white-space: normal; }
      .workflow-guide-box { background: #F7F9F9; border: 1px solid #D6DBDF; border-left: 5px solid #2E86C1; border-radius: 8px; padding: 10px 12px; margin: 10px 0; }
      .workflow-guide-box ol { padding-left: 18px; margin-bottom: 4px; }
      .workflow-guide-box li { margin-bottom: 4px; }
      .status-details { margin-top: 10px; background: #FFFFFF; border: 1px solid #F5B7B1; border-radius: 6px; padding: 6px 8px; }
      .status-details summary { cursor: pointer; font-weight: 600; color: #922B21; }
      .status-details pre { margin-top: 8px; margin-bottom: 0; white-space: pre-wrap; }
      .map-preview-box { padding: 10px; margin-bottom: 10px; background: #F8F9F9; border-left: 4px solid #16A085; border-radius: 6px; }
      .status-details { margin-top: 8px; }
      .map-display-note { font-size: 12px; color: #566573; margin-top: 4px; }
      .scenario-lab-box { background: #F8FBFF; border: 1px solid #D6EAF8; border-left: 5px solid #2E86C1; border-radius: 8px; padding: 12px; margin-bottom: 12px; }
      .scenario-card { background: #FFFFFF; border: 1px solid #D5DBDB; border-radius: 8px; padding: 10px; margin-bottom: 10px; }
      .scenario-preset-note { background: #F4F6F7; border-left: 4px solid #85929E; padding: 8px; border-radius: 6px; margin: 8px 0; }
      .scenario-card-polished { background: #FFFFFF; border: 1px solid #D5DBDB; border-radius: 12px; padding: 14px; margin-bottom: 12px; box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
      .scenario-card-polished h3 { margin-top: 0; margin-bottom: 4px; color: #1C2833; }
      .scenario-subtitle { color: #566573; font-size: 13px; margin-bottom: 10px; }
      .scenario-kpi-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; margin: 10px 0; }
      .scenario-kpi { background: #F8F9F9; border-radius: 8px; padding: 9px; border: 1px solid #EBEDEF; }
      .scenario-kpi-label { font-size: 11px; color: #566573; text-transform: uppercase; letter-spacing: 0.02em; }
      .scenario-kpi-value { font-size: 16px; font-weight: 700; color: #1C2833; word-break: break-word; }
      .scenario-interpretation { background: #FEF9E7; border-left: 4px solid #F1C40F; padding: 10px; margin-top: 10px; border-radius: 8px; }
      .scenario-tag { display: inline-block; background: #EAF2F8; color: #1B4F72; border-radius: 999px; padding: 3px 9px; margin: 2px 4px 2px 0; font-size: 12px; }
      .scenario-kpi-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; margin: 8px 0; }
      .scenario-kpi { background: #F8F9F9; border-radius: 6px; padding: 8px; }
      .scenario-kpi-label { font-size: 11px; color: #566573; text-transform: uppercase; }
      .scenario-kpi-value { font-size: 16px; font-weight: 700; color: #1C2833; }
      .scenario-interpretation { background: #FEF9E7; border-left: 4px solid #F1C40F; padding: 8px; margin-top: 8px; border-radius: 6px; }
      .scenario-card h4 { margin-top: 0; }
      .scenario-action-row { display: grid; grid-template-columns: repeat(4, 1fr); gap: 8px; margin-top: 8px; }
      .scenario-action-row .btn { width: 100%; white-space: normal; }
      .advanced-note { font-size: 12px; color: #566573; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToMainMap', function(message) {
        setTimeout(function() {
          var el = document.getElementById('map_panel_anchor') || document.getElementById('main_tabs');
          if (el) { el.scrollIntoView({behavior: 'smooth', block: 'start'}); }
        }, 450);
      });

      Shiny.addCustomMessageHandler('updateVisibleTabs', function(x) {
        function setTabVisible(label, visible) {
          var links = $('a[data-toggle=\\'tab\\'], a[data-bs-toggle=\\'tab\\']').filter(function() { return $(this).text().trim() === label; });
          links.each(function() { $(this).parent().toggle(!!visible); });
        }
        var advanced = x && x.advanced;
        var dynamicOn = x && x.dynamicOn;
        setTabVisible('Variant dynamics', dynamicOn);
        setTabVisible('Calibration & Representativeness', advanced);
        setTabVisible('Containment measures', advanced);
        setTabVisible('Containment Measures', advanced);
        setTabVisible('Age adjustment & cost', true);
        setTabVisible('Age Adjustment & Cost', true);
      });
    "))
  ),

  titlePanel(
    h2("Population epidemiological simulator with COVID reference comparator"),
    h4("Configurable simulation against a COVID reference - R/Shiny tool for teaching, planning and visualization", style = "color: #7F8C8D;")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Simulation parameters", style = "color: #34495E; font-weight: bold;"),
      selectInput(
        "active_scenario_mode",
        "Detail level:",
        choices = c(
          "Guided basic mode" = "guided",
          "Complete technical mode" = "complete"
        ),
        selected = "guided"
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        checkboxInput("show_technical_validation_options", "Show technical validation options", value = FALSE)
      ),
      div(class = "mode-box",
        conditionalPanel(
          condition = "input.active_scenario_mode == 'guided'",
          tags$b("Guided basic mode"),
          tags$p(class = "advanced-note", "Shows only the main scenario controls. Seed, calibration, air traffic, robustness and technical controls remain in complete mode.")
        ),
        conditionalPanel(
          condition = "input.active_scenario_mode == 'complete'",
          tags$b("Complete technical mode"),
          tags$p(class = "advanced-note", "Shows all technical controls: calibration, mobility, seed, robustness, dynamic parameters and diagnostics.")
        )
      ),
      div(class = "workflow-guide-box",
        conditionalPanel(
          condition = "input.active_scenario_mode == 'guided'",
          tags$b("Suggested order in basic mode"),
          tags$ol(
            tags$li("Choose the initial country or use the random-country button."),
            tags$li("Set initial expansion, exposed phase, active duration and average mortality."),
            tags$li("Choose whether advantage variants and containment measures are included."),
            tags$li("Set the simulation horizon and run the scenario."),
            tags$li("Explore the map, curves and comparator-based results.")
          )
        ),
        conditionalPanel(
          condition = "input.active_scenario_mode == 'complete'",
          tags$b("Suggested order in complete mode"),
          tags$ol(
            tags$li("Configure engine, age, seed and mobility when technical reproducibility is needed."),
            tags$li("Set the pathogen-like scenario and dynamic parameters."),
            tags$li("Enable calibration or robustness only if you will interpret diagnostics."),
            tags$li("Run the scenario and review maps, curves, reports and warnings."),
            tags$li("Use copyable reports to compare runs.")
          )
        )
      ),
      div(class = "fixed-ref-box",
        h4("COVID comparator", style = "color: #27AE60; font-weight: bold;"),
        conditionalPanel(
          condition = "input.active_scenario_mode == 'complete'",
          selectInput(
            "reference_comparator_type",
            "Reference comparator:",
            choices = c(
              "Automatic from active model" = "auto",
              "Basic Omicron SIRD RDS" = "basic",
              "Age-adjusted Omicron SIRD RDS" = "age_adjusted",
              "Age-adjusted Omicron SEIRD RDS" = "age_adjusted_seird"
            ),
            selected = "auto"
          )
        ),
        uiOutput("covid_reference_card"),
        tags$p(class = "small-note", "This comparator represents a theoretical SEIRD scenario with age effects applied to the model and international passenger traffic between countries. It describes the plausible model-based trajectory if the first variant had been Omicron and no containment measures had been applied at any point.")
      ),
      div(class = "simulation-action-box",
        tags$div(class = "primary-run-button",
          actionButton("run_simulation", "Run simulation", icon = icon("play"), class = "btn btn-danger btn-lg")
        ),
        checkboxInput("auto_play_after_run", "Automatically play animation after completion", value = TRUE),
        tags$div(class = "secondary-action-row",
          actionButton("reset_defaults", "Reset defaults", icon = icon("rotate-left"), class = "btn btn-default"),
          downloadButton("download_csv", "Download results CSV", class = "btn btn-default")
        ),
        conditionalPanel(
          condition = "input.active_scenario_mode == 'complete'",
          tags$div(style = "margin-top: 8px;",
            actionButton("apply_covid_values", "Apply Omicron reference to simulation", icon = icon("flask"), class = "btn btn-default", width = "100%")
          )
        ),
        tags$p(class = "small-note", "Runs the calculation and displays the scenario maps."),
        tags$details(class = "status-details",
          tags$summary("Last simulation status"),
          verbatimTextOutput("computation_status")
        )
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        h4("Model characteristics", style = "color: #34495E; font-weight: bold;"),
        selectInput(
          "model_structure",
          "Model type:",
          choices = c(
            "Basic aggregated model" = "basic",
            "Age-adjusted model" = "age_adjusted"
          ),
          selected = "age_adjusted"
        ),
        helpText("Technical validation options. Basic mode automatically uses the age-adjusted engine with differential mortality by age group."),
        selectInput(
          "disease_compartment_model",
          "Compartment structure:",
          choices = c(
            "SEIRD: includes prior exposed phase" = "SEIRD",
            "SIRD: direct transition to active phase" = "SIRD"
          ),
          selected = "SEIRD"
        ),
        conditionalPanel(
          condition = "input.model_structure == 'age_adjusted'",
          selectInput(
            "age_parameter_mode",
            "Age-parameter mode:",
            choices = c(
              "Technical validation: same mortality in all groups" = "neutral",
              "Recommended: age-differential mortality" = "differential_cfr"
            ),
            selected = "differential_cfr"
          ),
          numericInput("age_cfr_scale", "Global age-mortality adjustment:", value = 1, min = 0, max = 10, step = 0.1),
          selectInput(
            "age_distribution_mode",
            "Age distribution:",
            choices = c(
              "Country-specific age pyramids" = "country_specific",
              "Average global pyramid" = "global_average"
            ),
            selected = "country_specific"
          ),
          helpText("Country-specific mode reads country_age_distribution_wpp2024_6groups.rds/csv when available. The global pyramid remains a technical validation option.")
        ),
        hr()
      ),

      h4("Country where the infection originates", style = "color: #2E86C1; font-weight: bold;"),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        selectInput(
          "starting_country_mode",
          "Advanced initial-country selection mode:",
          choices = c(
            "Random on each run" = "random_each_run",
            "Stable random with seed" = "seeded_stable",
            "Manually selected country" = "manual"
          ),
          selected = "manual"
        )
      ),
      selectInput("starting_country", "Country where the infection originates:", choices = COUNTRIES_LIST, selected = sample(COUNTRIES_LIST, 1)),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'guided'",
        actionButton("random_start_country", "Choose random country", icon = icon("dice"), class = "btn btn-outline-primary btn-sm"),
        helpText("In basic mode you can choose the country manually or use this button to select one at random. The seed remains hidden, but is stored internally for later comparison and export.")
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        helpText("In complete mode you can use manual, random, or stable random selection with visible seed.")
      ),
      numericInput("initial_infected", "Initial number of cases:", value = 5, min = 1, max = 100000, step = 1),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        selectInput(
          "air_travel_scenario",
          "International air-traffic scenario:",
          choices = c(
            "Restricted traffic (25% of reference)" = "restricted",
            "Reduced traffic (60% of reference)" = "reduced",
            "Reference traffic (100%)" = "reference",
            "High / holiday traffic (150%)" = "high"
          ),
          selected = "reference"
        ),
        numericInput("import_establishment_probability", "Imported-establishment opportunity modifier:", value = 1.00, min = 0, max = 3, step = 0.05),
        helpText("Multiplier applied to the aggregate opportunity for an international entry to generate local transmission. 1.0 adds no extra attenuation.")
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete' && input.starting_country_mode != 'random_each_run'",
        textInput("stochastic_seed", "Seed for reproducibility (blank = random):", value = ""),
        helpText("Use a fixed seed for reproducible stochastic simulation runs. This is useful even when the starting country is manually selected, because other stochastic components can still vary. If 'Seeded random, stable' is selected with a blank seed, the run is not reproducible and this will be flagged in the report.")
      ),
      hr(),

      h4("Pathogen-like scenario", style = "color: #E74C3C; font-weight: bold;"),
      numericInput("R0", "Initial expansion capacity (R0/Rt):", value = 5.0, min = 0.5, max = 15.0, step = 0.1),
      helpText("Initial aggregate expansion value. Higher values mean faster expansion when no reducing measures are applied."),
      numericInput("infectious_period_days", "Active-phase duration (days):", value = 20, min = 2, max = 60, step = 1),
      numericInput(
        "exposed_period_days",
        "Exposed phase before active transmission (days):",
        value = 4,
        min = 1,
        max = 14,
        step = 1
      ),
      helpText("SEIRD uses E = exposed phase: people already incorporated into the modelled infection process, but not yet in the active/transmitting phase. The default 4-day value is an Omicron-like teaching/planning assumption; try 3-5 days for sensitivity analysis."),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        selectInput(
          "infectiousness_profile",
          "Transmission profile during that period:",
          choices = c(
            "Intermediatete peak" = "mid",
            "Early peak" = "early",
            "Late peak" = "late",
            "Flat distribution" = "flat"
          ),
          selected = "mid"
        ),
        helpText("Defines how transmission contribution is distributed during the active period. Basic mode uses the default value.")
      ),
      numericInput("mortality_rate", "Average mortality (%):", value = 1.0, min = 0, max = 100, step = 0.1),
      helpText("Target average among resolved cases. The app distributes it by age using each country's age structure. Very high values are treated as extreme scenarios."),
      hr(),

      h4("Emergence of advantage variants", style = "color: #9B59B6; font-weight: bold;"),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'guided'",
        selectInput(
          "dynamic_module_scenario",
          "Emergence of advantage variants:",
          choices = c(
            "Off" = "off",
            "Low" = "low",
            "Intermediate" = "reference",
            "High" = "high"
          ),
          selected = "off"
        ),
        helpText("Controls aggregate emergence of variants with population-level advantage. It is stochastic: a single run does not guarantee strict ordering between low, intermediate and high.")
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        selectInput(
          "evolution_module_mode",
          "Dynamic variant module:",
          choices = c(
            "Off" = "off",
            "Active for this simulation" = "active"
          ),
          selected = "active"
        ),
        helpText("Complete mode shows technical parameters of the dynamic module. It applies only to the configurable simulation."),
        conditionalPanel(
          condition = "input.evolution_module_mode == 'active'",
          numericInput("mutation_rate_per_replication", "Macro-effective replacement-opportunity rate:", value = 5e-7, min = 1e-9, max = 1e-3, step = 1e-7),
          helpText("Aggregate scenario parameter. It is not a molecular per-base rate; it scales population-level replacement opportunities inside the model."),
          numericInput("effective_mutation_targets", "Population-opportunity multiplier:", value = 30, min = 1, max = 300, step = 1),
          selectInput(
            "variant_emergence_calibration",
            "Macroscopic variant-emergence calibration:",
            choices = c(
              "Conservative" = "conservative",
              "Macro reference for RNA viruses" = "reference",
              "High incidence / pandemic scale" = "high"
            ),
            selected = "reference"
          ),
          numericInput("maximum_variant_R0_multiplier", "Maximum R0/Rt multiplier versus initial variant:", value = 1.65, min = 1.0, max = 3.0, step = 0.05),
          helpText("This limit avoids unrealistic cumulative inflation. Effective R0/Rt is frequency-weighted by variants. It is a scenario bound, not a prediction.")
        )
      ),
      hr(),

      h4("Containment measures", style = "color: #7D3C98; font-weight: bold;"),
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
        selectizeInput("containment_affected_continents", "Affected continents:", choices = CONTINENTS_LIST, selected = "Europe", multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.containment_geographic_scope == 'countries'",
        selectizeInput("containment_affected_countries", "Affected countries:", choices = COUNTRIES_LIST, selected = "Norway", multiple = TRUE)
      ),
      numericInput("containment_start_day", "Start day of measures:", value = 210, min = 0, max = 1095, step = 1),
      numericInput("containment_end_day", "End day of measures:", value = 240, min = 0, max = 1095, step = 1),
      conditionalPanel(
        condition = "input.containment_preset == 'custom'",
        numericInput("containment_transmission_reduction", "Local transmission reduction (%):", value = 50, min = 0, max = 100, step = 1),
        numericInput("containment_mobility_reduction", "Between-country mobility reduction (%):", value = 50, min = 0, max = 100, step = 1)
      ),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        textInput("containment_label", "Measure-period label:", value = "Containment period")
      ),
      helpText("Definitions: Mild = partial reduction in transmission and mobility. Moderate = intermediate package. Strong = temporary high-intensity reduction. Measures apply only to the configurable simulation, not to the comparator."),
      hr(),

      h4("Simulation horizon", style = "color: #E74C3C; font-weight: bold;"),
      numericInput("simulation_days", "Simulated days:", value = 365, min = 30, max = 1095, step = 30),
      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        radioButtons("global_plot_scale", "Plot scale:", choices = c("Persons" = "persons", "Percentage of world population" = "percent"), selected = "persons")
      ),
      hr(),

      conditionalPanel(
        condition = "input.active_scenario_mode == 'complete'",
        h4("Representativeness and calibration", style = "color: #1F618D; font-weight: bold;"),
        checkboxInput("enable_calibration_targets", "Show calibration diagnostics", value = TRUE),
        numericInput("calibration_population", "Reference population for calibration:", value = WORLD_POPULATION, min = 1, step = 1000000),
        numericInput("observed_positive_cumulative", "Observed cumulative positives (absolute number):", value = NA, min = 0, step = 100000),
        numericInput("case_detection_fraction", "User-specified detection fraction (0-1):", value = 0.20, min = 0.0001, max = 1.0, step = 0.01),
        checkboxInput("estimate_detection_from_serology", "Estimate detection fraction using seroprevalence and positives", value = TRUE),
        numericInput("observed_positive_day", "Observation day for cumulative positives:", value = 60, min = 0, max = 1095, step = 1),
        numericInput("positive_lag_days", "Lag from underlying event to positive record (days; default 5, plausible range 3-7):", value = 5, min = 0, max = 60, step = 1),
        numericInput("observed_seroprevalence_percent", "Observed seroprevalence (% of reference population):", value = NA, min = 0, max = 100, step = 0.1),
        numericInput("observed_seroprevalence_day", "Observation day for seroprevalence:", value = 90, min = 0, max = 1095, step = 1),
        numericInput("seroprevalence_lag_days", "Lag from underlying event to serology signal (days; default 26, plausible range 18-30):", value = 26, min = 0, max = 90, step = 1),
        checkboxInput("enable_day0_date_estimation", "Estimate calendar date for simulated day 0", value = FALSE),
        dateInput("observed_positive_calendar_date", "Calendar date of cumulative-positive observation:", value = NULL, format = "yyyy-mm-dd"),
        dateInput("observed_seroprevalence_calendar_date", "Calendar date of seroprevalence observation:", value = NULL, format = "yyyy-mm-dd"),
        helpText("Diagnostic only: these inputs do not change the simulated trajectory. They compare positives and seroprevalence with lag-adjusted cumulative outputs and estimate an implied detection fraction."),
        hr()
      ),

      h4("Explicit model assumptions", style = "font-weight: bold;"),
      tableOutput("assumptions_table")
    ),

    mainPanel(
      tabsetPanel(id = "main_tabs", type = "tabs",
        tabPanel("World map",
          h4("Initial connectivity before simulation"),
          div(class = "map-preview-box",
            checkboxInput("show_connectivity_preview", "Show initial connectivity of selected country", value = TRUE),
            conditionalPanel(
              condition = "input.show_connectivity_preview == true",
              tags$p(class = "small-note", "Shows the 15 main connections of the initial country. Line width represents the relative aggregate mobility weight used by the model."),
              leafletOutput("connectivity_preview_map", height = "340px")
            )
          ),
          h4("Geographic evolution during simulation"),
          div(
            style = "padding: 10px; margin-bottom: 10px; background: #F8F9F9; border-left: 4px solid #3498DB;",
            sliderInput("time_slider", "Simulation day:", min = 0, max = 365, value = 0, step = 1),
            actionButton("play_animation", "Start animation"),
            actionButton("pause_animation", "Pause animation")
          ),
          div(id = "map_panel_anchor",
            fluidRow(
              column(6, h4("Active by country"), leafletOutput("world_map_infected", height = "520px")),
              column(6, h4("Cumulative deaths by country"), leafletOutput("world_map_deaths", height = "520px"))
            )
          )
        ),

        tabPanel("Global results",
          fluidRow(column(12, plotlyOutput("plot_infected", height = "400px"))),
          fluidRow(column(12, plotlyOutput("plot_deaths", height = "400px"))),
          fluidRow(column(12, plotlyOutput("plot_recovered", height = "400px"))),
          hr(),
          h4("Peak timing"),
          tableOutput("global_peak_summary_table")
        ),

        tabPanel("Scenario lab",
          div(class = "scenario-lab-box",
            h4("Save and compare scenarios"),
            tags$p(class = "small-note", "Save the current simulation, restore configurations and compare main results. Exportable cards are designed for teaching, scenario discussion and meso-management."),
            selectInput(
              "scenario_preset_template",
              "Start from a preset scenario:",
              choices = c(
                "Keep current settings" = "none",
                "No containment baseline" = "no_containment",
                "Early moderate containment" = "early_moderate",
                "Late moderate containment" = "late_moderate",
                "Strong containment" = "strong_containment",
                "High replacement pressure" = "high_replacement",
                "High mobility without containment" = "high_mobility"
              ),
              selected = "none"
            ),
            actionButton("apply_scenario_preset_template", "Apply preset", icon = icon("wand-magic-sparkles"), class = "btn btn-default"),
            tags$p(class = "scenario-preset-note", "Preset scenarios are starting points for exploration. They change visible controls but do not alter the model engine."),
            textInput("scenario_name", "Scenario name:", value = "Scenario A"),
            selectInput(
              "scenario_tag",
              "Scenario label:",
              choices = c(
                "Baseline" = "Baseline",
                "Early containment" = "Early containment",
                "Late containment" = "Late containment",
                "High mobility" = "High mobility",
                "Variant pressure" = "Variant pressure",
                "Sensitivity analysis" = "Sensitivity analysis",
                "Teaching example" = "Teaching example"
              ),
              selected = "Baseline"
            ),
            div(class = "scenario-action-row",
              actionButton("save_current_scenario", "Save scenario", icon = icon("save"), class = "btn btn-primary"),
              actionButton("duplicate_selected_scenario", "Duplicate selected", icon = icon("copy"), class = "btn btn-default"),
              actionButton("restore_selected_scenario", "Restore configuration", icon = icon("rotate-left"), class = "btn btn-default"),
              actionButton("delete_selected_scenario", "Delete selected", icon = icon("trash"), class = "btn btn-default")
            ),
            br(),
            selectInput("selected_saved_scenario", "Saved scenario:", choices = character(0)),
            div(class = "scenario-action-row",
              downloadButton("download_scenario_card_html", "Export HTML card"),
              downloadButton("download_scenario_config_json", "Export JSON"),
              downloadButton("download_scenario_summary_csv", "Export CSV table"),
              actionButton("compare_saved_scenario", "Compare with current", icon = icon("chart-line"), class = "btn btn-default")
            ),
            br(),
            fileInput("import_scenario_config_json", "Import JSON configuration:", accept = c(".json", "application/json")),
            actionButton("import_scenario_config", "Load imported configuration", icon = icon("upload"), class = "btn btn-default")
          ),
          div(class = "scenario-lab-box",
            h4("Exploration prompts"),
            tags$ul(
              tags$li("Can you delay the active peak without increasing final deaths?"),
              tags$li("What changes more when containment starts late: peak timing or final mortality?"),
              tags$li("Which starting countries accelerate global spread under the same seed?"),
              tags$li("How much does the exposed compartment change the curve compared with SIRD?"),
              tags$li("Does stronger replacement pressure change one run or the distribution across seeds?")
            )
          ),
          fluidRow(
            column(5,
              h4("Selected scenario card"),
              uiOutput("selected_scenario_card")
            ),
            column(7,
              h4("A/B/C comparison"),
              tableOutput("scenario_comparison_table")
            )
          ),
          hr(),
          h4("Compared curves"),
          div(class = "scenario-action-row",
            downloadButton("download_scenario_comparison_csv", "Export comparison CSV"),
            downloadButton("download_scenario_comparison_html", "Export comparison HTML")
          ),
          plotlyOutput("scenario_comparison_plot", height = "420px"),
          hr(),
          h4("Scenario library"),
          tableOutput("scenario_library_table")
        ),

        tabPanel("Variant dynamics",
          h4("Variant Frequency Over Time"),
          plotlyOutput("evolution_stacked_area", height = "420px"),
          hr(),
          h4("Candidate Mutations and Established Variants"),
          verbatimTextOutput("fitness_evolution"),
          hr(),
          h4("Variant emergence plausibility check"),
          h4("Dynamic diagnostics"),
          tableOutput("dynamic_diagnostics_table"),
          hr(),
          h4("Dynamic preset robustness across seeds"),
          tags$p(class = "small-note", "Optional diagnostic: runs Low, Reference and High opportunity-pressure presets across multiple seeds. This evaluates presets in distribution, not as a single deterministic run."),
          numericInput("dynamic_robustness_replicates", "Replicates per preset:", value = 10, min = 3, max = 50, step = 1),
          actionButton("run_dynamic_robustness", "Run dynamic robustness check"),
          tableOutput("dynamic_robustness_table"),
          h4("Global High vs Reference comparison"),
          tableOutput("dynamic_robustness_global_table"),
          verbatimTextOutput("dynamic_robustness_report"),
          h4("Plausibility diagnostic"),
          tableOutput("variant_plausibility_table"),
          tags$p(class = "small-note", "This is a macroscopic calibration diagnostic. It does not model biological modification, laboratory selection, or mechanisms of altering a pathogen."),
          hr(),
          h4("Selective sweep analysis"),
          tableOutput("selective_sweep_table")
        ),

        tabPanel("Country-level spread",
          h4("Infection distribution by country"),
          plotlyOutput("country_distribution", height = "420px"),
          hr(),
          h4("Countries reached over time"),
          plotlyOutput("country_spread_timeline", height = "320px"),
          hr(),
          h4("Top affected countries"),
          tableOutput("top_countries_table"),
          conditionalPanel(
            condition = "input.active_scenario_mode == 'complete'",
            hr(),
            h4("Top international passenger corridors used by the model"),
            tableOutput("top_passenger_routes_table"),
            hr(),
            h4("Diffusion and representation diagnostics"),
            tableOutput("spread_diagnostic_table"),
            tableOutput("import_diagnostic_table"),
            tags$p(class = "small-note", "These tables compare what is stored in the simulation arrays with what is eligible for display at the selected day. They are intended to distinguish calculation failures from filtering/representation failures.")
          )
        ),

        tabPanel("Age adjustment & cost",
          conditionalPanel(
            condition = "input.active_scenario_mode == 'complete'",
            h4("Age-group parameters used by the model"),
            tableOutput("age_parameter_table"),
            hr()
          ),
          h4("Age-group summary after simulation"),
          tableOutput("age_summary_table"),
          conditionalPanel(
            condition = "input.active_scenario_mode == 'complete'",
            hr(),
            h4("Computational cost profile"),
            tableOutput("cost_profile_table"),
            hr(),
            h4("Copy-paste evaluation report"),
            verbatimTextOutput("evaluation_report")
          )
        ),

        tabPanel("Calibration & Representativeness",
          h4("Lag-adjusted calibration diagnostics"),
          tableOutput("calibration_diagnostic_table"),
          hr(),
          h4("Detection fraction and calendar-alignment diagnostics"),
          tableOutput("detection_fraction_table"),
          hr(),
          h4("Copy-paste calibration report"),
          verbatimTextOutput("calibration_report"),
          tags$p(class = "small-note", "This tab only compares simulated aggregate outputs against user-provided reference points. It does not alter the trajectory.")
        ),

        tabPanel("Containment measures",
          h4("Scenario-level containment definitions"),
          tableOutput("containment_preset_table"),
          hr(),
          h4("Containment schedule used in current run"),
          tableOutput("containment_schedule_table"),
          hr(),
          h4("Horizon / unresolved active diagnostic"),
          tableOutput("horizon_diagnostic_table"),
          tags$p(class = "small-note", "If the scenario ends with active cases, comparisons of final deaths can be partly censored by the selected horizon."),
          hr(),
          h4("Copy-paste containment report"),
          verbatimTextOutput("containment_report"),
          tags$p(class = "small-note", "Containment measures are applied only to the active user scenario. The Omicron RDS comparator is not modified.")
        ),

        tabPanel("Simulator logic",
          h4("How the simulator works"),
          tags$p(class = "small-note", "This diagram summarizes the logic of the tool and highlights what is visible in Guided basic mode versus Complete advanced mode."),
          div(class = "logic-box both",
            h4("1. COVID comparator"),
            tags$span(class = "logic-chip both", "Basic"),
            tags$span(class = "logic-chip both", "Advanced"),
            tags$p("A COVID reference is loaded from RDS. It provides a stable comparison baseline and is not recalculated when the simulation runs.")
          ),
          div(class = "logic-arrow", HTML("&#8595;")),
          div(class = "logic-box guided",
            h4("2. Active scenario setup"),
            tags$span(class = "logic-chip guided", "Basic"),
            tags$span(class = "logic-chip advanced", "Advanced"),
            tags$p("The user defines the configurable simulation. In Guided basic mode, only the clearest scenario controls remain visible. In Complete advanced mode, all technical controls remain available."),
            fluidRow(
              column(6,
                tags$b("Visible in basic mode"),
                tags$ul(
                  tags$li("Initial country"),
                  tags$li("Initial expansion capacity"),
                  tags$li("Active case duration"),
                  tags$li("Average mortality (%)"),
                  tags$li("Emergence of variants with advantage"),
                  tags$li("Containment intensity"),
                  tags$li("Where containment is applied")
                )
              ),
              column(6,
                tags$b("Visible only in advanced mode"),
                tags$ul(
                  tags$li("Random seed and robustness checks"),
                  tags$li("Calibration inputs and lags"),
                  tags$li("Passenger traffic scenario and import settings"),
                  tags$li("Detailed dynamic-replacement controls"),
                  tags$li("Technical validation options")
                )
              )
            )
          ),
          div(class = "logic-arrow", HTML("&#8595;")),
          div(class = "logic-box both",
            h4("3. Core simulation engine"),
            tags$span(class = "logic-chip both", "Basic"),
            tags$span(class = "logic-chip both", "Advanced"),
            tags$p("The app runs a macroscopic population-level SIRD/SEIRD simulation with geographic spread, country-specific age structure, age-weighted CFR, optional containment and optional variant replacement dynamics.")
          ),
          div(class = "logic-arrow", HTML("&#8595;")),
          div(class = "logic-box advanced",
            h4("4. Optional diagnostics"),
            tags$span(class = "logic-chip advanced", "Advanced"),
            tags$p("Advanced mode adds deeper diagnostics such as representation checks, seed robustness, calibration comparisons, dynamic plausibility summaries and unresolved-horizon checks.")
          ),
          div(class = "logic-arrow", HTML("&#8595;")),
          div(class = "logic-box both",
            h4("5. Outputs for exploration"),
            tags$span(class = "logic-chip both", "Basic"),
            tags$span(class = "logic-chip both", "Advanced"),
            tags$p("Results are displayed through world maps, global time-series plots, country tables, age summaries, containment summaries and comparator-based interpretation outputs. The goal is scenario exploration, teaching and meso-management reflection, not exact prediction.")
          ),
          hr(),
          h4("Resumen de disponibilidad"),
          tableOutput("logic_availability_table")
        ),

        tabPanel("Scope, assumptions & limitations",
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
          tags$p(class = "small-note", "Links are provided when a stable public source is available. Scenario presets remain labelled as such when the literature supports a range rather than a single universal value."),
          hr(),
          h4("SEIRD and exposed-compartment assumptions"),
          tableOutput("seird_assumptions_table"),
          tags$p(class = "small-note", "The E compartment is a population-level timing state. It represents the delay between modelled exposure/infection and entry into the active transmitting compartment; it is not an individual diagnosis or laboratory state."),
          hr(),
          h4("Dynamic module assumptions"),
          tableOutput("dynamic_assumptions_table"),
          tags$p(class = "small-note", "Guided dynamic presets are scenario settings. Adaptive-space saturation is active for guided dynamic presets: expressed advantage is reduced as the effective R0 approaches the scenario cap. Detailed parameters remain in Complete advanced mode and are labelled as technical defaults pending dedicated literature review."),
          hr(),
          h4("Reference comparator metadata"),
          tableOutput("reference_comparator_metadata_table"),
          hr(),
          h4("Copy-paste scope report"),
          verbatimTextOutput("model_scope_report")
        ),

        tabPanel("R0 Analysis",
          fluidRow(column(12, plotlyOutput("plot_r0_comparison", height = "420px"))),
          fluidRow(column(12, plotlyOutput("plot_cfr_comparison", height = "360px"))),
          h4("R0 calculations"),
          verbatimTextOutput("r0_details")
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  observe({
    advanced <- identical(input$active_scenario_mode, "complete")
    dynamic_on <- if (advanced) {
      identical(input$evolution_module_mode, "active")
    } else {
      !identical(input$dynamic_module_scenario, "off")
    }
    session$sendCustomMessage("updateVisibleTabs", list(advanced = advanced, dynamicOn = dynamic_on))
    hidden_in_guided <- c("Calibration & Representativeness", "Containment measures")
    if (!advanced && !is.null(input$main_tabs) && input$main_tabs %in% hidden_in_guided) {
      updateTabsetPanel(session, "main_tabs", selected = "World map")
    }
    if (!dynamic_on && !is.null(input$main_tabs) && identical(input$main_tabs, "Variant dynamics")) {
      updateTabsetPanel(session, "main_tabs", selected = "World map")
    }
  })

  observeEvent(input$active_scenario_mode, {
    if (identical(input$active_scenario_mode, "guided")) {
      updateSelectInput(session, "model_structure", selected = "age_adjusted")
      updateSelectInput(session, "age_parameter_mode", selected = "differential_cfr")
    }
  }, ignoreInit = FALSE)

  animation_running <- reactiveVal(FALSE)
  sim_data <- reactiveValues()
  scenario_library <- reactiveValues(items = list(), counter = 0, compare_id = NULL)

  scenario_safe_value <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0) return(default)
    x
  }

  capture_current_scenario_config <- function() {
    list(
      app_file = "app_epidemiologic_v16_scenario_lab_step6b_basic_exposed_parameter.R",
      saved_at = as.character(Sys.time()),
      scenario_tag = if (!is.null(input$scenario_tag)) input$scenario_tag else "Baseline",
      active_scenario_mode = scenario_safe_value(input$active_scenario_mode),
      model_structure = scenario_safe_value(input$model_structure),
      disease_compartment_model = if (!is.null(input$disease_compartment_model)) input$disease_compartment_model else "SEIRD",
      exposed_period_days = if (!is.null(input$exposed_period_days)) input$exposed_period_days else 4,
      age_parameter_mode = scenario_safe_value(input$age_parameter_mode),
      age_distribution_mode = if (!is.null(input$age_distribution_mode)) input$age_distribution_mode else "country_specific",
      starting_country = scenario_safe_value(input$starting_country),
      starting_country_mode = if (!is.null(input$starting_country_mode)) input$starting_country_mode else "manual",
      stochastic_seed = scenario_safe_value(input$stochastic_seed, ""),
      simulation_days = scenario_safe_value(input$simulation_days),
      R0 = scenario_safe_value(input$R0),
      infectious_period_days = scenario_safe_value(input$infectious_period_days),
      mortality_rate = scenario_safe_value(input$mortality_rate),
      dynamic_scenario = if (!is.null(input$dynamic_scenario)) input$dynamic_scenario else "off",
      containment_preset = if (!is.null(input$containment_preset)) input$containment_preset else "none",
      containment_start_day = if (!is.null(input$containment_start_day)) input$containment_start_day else NA,
      containment_end_day = if (!is.null(input$containment_end_day)) input$containment_end_day else NA,
      containment_geographic_scope = if (!is.null(input$containment_geographic_scope)) input$containment_geographic_scope else "global",
      air_travel_scenario = if (!is.null(input$air_travel_scenario)) input$air_travel_scenario else "reference",
      import_establishment_probability = if (!is.null(input$import_establishment_probability)) input$import_establishment_probability else 1,
      reference_comparator_type = if (!is.null(input$reference_comparator_type)) input$reference_comparator_type else "auto"
    )
  }

  scenario_summary_from_results <- function(name, config, results) {
    if (is.null(results) || is.null(results$data)) return(NULL)
    d <- results$data
    peak_idx <- which.max(d$I)
    peak_exposed_idx <- if ("E" %in% names(d)) which.max(d$E) else NA_integer_
    data.frame(
      Name = name,
      Label = if (!is.null(config$scenario_tag)) config$scenario_tag else "Baseline",
      Model = if (!is.null(results$compartment_model)) results$compartment_model else config$disease_compartment_model,
      StartCountry = scenario_safe_value(results$starting_country_used, config$starting_country),
      Days = config$simulation_days,
      R0 = config$R0,
      ExposedDays = if (!is.null(results$exposed_period_days)) results$exposed_period_days else config$exposed_period_days,
      ActivePhaseDays = config$infectious_period_days,
      MortalityPercent = config$mortality_rate,
      PeakDay = d$time[peak_idx],
      PeakActive = d$I[peak_idx],
      PeakActivePercentWorld = 100 * d$I[peak_idx] / WORLD_POPULATION,
      PeakExposedDay = if (!is.na(peak_exposed_idx)) d$time[peak_exposed_idx] else NA,
      PeakExposed = if (!is.na(peak_exposed_idx) && "E" %in% names(d)) d$E[peak_exposed_idx] else NA,
      FinalActive = tail(d$I, 1),
      FinalDeaths = tail(d$D, 1),
      FinalRecovered = tail(d$R, 1),
      CountriesReached = if (!is.null(results$first_reached_day)) sum(!is.na(results$first_reached_day)) else NA,
      Comparator = if (!is.null(results$cost_profile$rds_reference)) basename(results$cost_profile$rds_reference) else NA,
      StructureWarning = if (!is.null(results$cost_profile$comparator_structure_warning)) results$cost_profile$comparator_structure_warning else NA,
      stringsAsFactors = FALSE
    )
  }

  build_scenario_interpretation <- function(summary_row, comparator_row = NULL) {
    if (is.null(summary_row) || nrow(summary_row) == 0) return("There is not enough information to interpret the scenario.")
    base <- paste0(
      "The scenario reaches its active peak on day ", summary_row$PeakDay,
      " con ", format(round(summary_row$PeakActive), big.mark = ","),
      " active persons."
    )
    if (!is.null(comparator_row) && nrow(comparator_row) > 0) {
      delta_day <- summary_row$PeakDay - comparator_row$PeakDay
      delta_peak <- summary_row$PeakActive - comparator_row$PeakActive
      delta_deaths <- summary_row$FinalDeaths - comparator_row$FinalDeaths
      timing <- ifelse(delta_day > 0, paste0("later (+", delta_day, " days)"), ifelse(delta_day < 0, paste0("earlier (", delta_day, " days)"), "on the same day"))
      peak_txt <- ifelse(delta_peak > 0, paste0("higher (+", format(round(delta_peak), big.mark = ","), ")"), ifelse(delta_peak < 0, paste0("lower (", format(round(delta_peak), big.mark = ","), ")"), "similar"))
      death_txt <- ifelse(delta_deaths > 0, paste0("more deaths (+", format(round(delta_deaths), big.mark = ","), ")"), ifelse(delta_deaths < 0, paste0("fewer deaths (", format(round(delta_deaths), big.mark = ","), ")"), "similar final mortality"))
      return(paste(base, "Compared with the comparator, the peak occurs", timing, ", the active peak is", peak_txt, "and the simulation ends with", death_txt, "."))
    }
    base
  }

  current_comparison_rows <- reactive({
    rows <- list()
    if (!is.null(sim_data$hantavirus$data)) {
      cfg <- capture_current_scenario_config()
      rows[["actual"]] <- scenario_summary_from_results("Current simulation", cfg, sim_data$hantavirus)
    }
    compare_id <- scenario_library$compare_id
    if (!is.null(compare_id) && !is.null(scenario_library$items[[compare_id]])) {
      rows[["guardado"]] <- scenario_library$items[[compare_id]]$summary
    }
    if (!is.null(sim_data$covid$data)) {
      covid_like <- list(
        data = sim_data$covid$data,
        first_reached_day = sim_data$covid$first_reached_day,
        compartment_model = if (!is.null(sim_data$covid$compartment_model)) sim_data$covid$compartment_model else "SIRD",
        exposed_period_days = if (!is.null(sim_data$covid$exposed_period_days)) sim_data$covid$exposed_period_days else NA,
        cost_profile = list(rds_reference = if (!is.null(sim_data$covid$reference_file)) sim_data$covid$reference_file else "comparator", comparator_structure_warning = "reference")
      )
      cfg <- list(simulation_days = input$simulation_days, R0 = 4.25, exposed_period_days = NA, infectious_period_days = NA, mortality_rate = NA, starting_country = "South Africa", disease_compartment_model = covid_like$compartment_model)
      rows[["comparator"]] <- scenario_summary_from_results("Comparator", cfg, covid_like)
    }
    if (length(rows) == 0) return(data.frame())
    out <- dplyr::bind_rows(rows)
    comp <- out[out$Name == "Comparator", , drop = FALSE]
    if (nrow(comp) > 0) {
      out$DeltaPeakDayVsComparator <- out$PeakDay - comp$PeakDay[1]
      out$DeltaPeakActiveVsComparator <- out$PeakActive - comp$PeakActive[1]
      out$DeltaFinalDeathsVsComparator <- out$FinalDeaths - comp$FinalDeaths[1]
    } else {
      out$DeltaPeakDayVsComparator <- NA
      out$DeltaPeakActiveVsComparator <- NA
      out$DeltaFinalDeathsVsComparator <- NA
    }
    out
  })

  update_scenario_choices <- function() {
    ids <- names(scenario_library$items)
    choices <- if (length(ids) == 0) character(0) else setNames(ids, vapply(scenario_library$items, function(x) x$name, character(1)))
    updateSelectInput(session, "selected_saved_scenario", choices = choices, selected = if (length(ids) > 0) tail(ids, 1) else character(0))
  }

  apply_scenario_config <- function(cfg) {
    if (is.null(cfg)) return(invisible(FALSE))
    if (!is.null(cfg$active_scenario_mode)) updateRadioButtons(session, "active_scenario_mode", selected = cfg$active_scenario_mode)
    if (!is.null(cfg$model_structure)) updateSelectInput(session, "model_structure", selected = cfg$model_structure)
    if (!is.null(cfg$disease_compartment_model)) updateSelectInput(session, "disease_compartment_model", selected = cfg$disease_compartment_model)
    if (!is.null(cfg$exposed_period_days)) updateNumericInput(session, "exposed_period_days", value = cfg$exposed_period_days)
    if (!is.null(cfg$age_parameter_mode)) updateSelectInput(session, "age_parameter_mode", selected = cfg$age_parameter_mode)
    if (!is.null(cfg$age_distribution_mode)) updateSelectInput(session, "age_distribution_mode", selected = cfg$age_distribution_mode)
    if (!is.null(cfg$starting_country)) updateSelectInput(session, "starting_country", selected = cfg$starting_country)
    if (!is.null(cfg$starting_country_mode)) updateRadioButtons(session, "starting_country_mode", selected = cfg$starting_country_mode)
    if (!is.null(cfg$stochastic_seed)) updateTextInput(session, "stochastic_seed", value = as.character(cfg$stochastic_seed))
    if (!is.null(cfg$simulation_days)) updateNumericInput(session, "simulation_days", value = cfg$simulation_days)
    if (!is.null(cfg$R0)) updateNumericInput(session, "R0", value = cfg$R0)
    if (!is.null(cfg$infectious_period_days)) updateNumericInput(session, "infectious_period_days", value = cfg$infectious_period_days)
    if (!is.null(cfg$mortality_rate)) updateNumericInput(session, "mortality_rate", value = cfg$mortality_rate)
    if (!is.null(cfg$dynamic_scenario)) updateSelectInput(session, "dynamic_scenario", selected = cfg$dynamic_scenario)
    if (!is.null(cfg$containment_preset)) updateSelectInput(session, "containment_preset", selected = cfg$containment_preset)
    if (!is.null(cfg$containment_start_day) && is.finite(cfg$containment_start_day)) updateNumericInput(session, "containment_start_day", value = cfg$containment_start_day)
    if (!is.null(cfg$containment_end_day) && is.finite(cfg$containment_end_day)) updateNumericInput(session, "containment_end_day", value = cfg$containment_end_day)
    if (!is.null(cfg$containment_geographic_scope)) updateSelectInput(session, "containment_geographic_scope", selected = cfg$containment_geographic_scope)
    if (!is.null(cfg$air_travel_scenario)) updateSelectInput(session, "air_travel_scenario", selected = cfg$air_travel_scenario)
    if (!is.null(cfg$import_establishment_probability)) updateNumericInput(session, "import_establishment_probability", value = cfg$import_establishment_probability)
    if (!is.null(cfg$reference_comparator_type)) updateSelectInput(session, "reference_comparator_type", selected = cfg$reference_comparator_type)
    if (!is.null(cfg$scenario_tag)) updateSelectInput(session, "scenario_tag", selected = cfg$scenario_tag)
    invisible(TRUE)
  }

  computation_status <- reactiveVal("Waiting for calculation.")
  dynamic_robustness_results <- reactiveVal(NULL)
  # Hidden seed used by guided mode. It is not exposed in the basic UI,
  # but it is stored internally so future scenario JSON exports can remain reproducible.
  guided_internal_seed <- reactiveVal(sample.int(.Machine$integer.max - 1L, 1L))
  # Tracks whether the basic-mode country was explicitly selected or generated
  # with the simple random-country button. Useful for future scenario JSON export.
  guided_country_selection_source <- reactiveVal("manual_or_default")
  guided_random_country_clicks <- reactiveVal(0L)
  output$computation_status <- renderText(computation_status())

  parse_optional_seed <- function(seed_value) {
    if (is.null(seed_value) || length(seed_value) == 0 || is.na(seed_value)) return(NULL)
    seed_value <- trimws(as.character(seed_value))
    if (!nzchar(seed_value)) return(NULL)
    parsed <- suppressWarnings(as.integer(seed_value))
    if (is.na(parsed)) return(NULL)
    parsed
  }

  dynamic_preset_values <- function(preset) {
    preset <- if (is.null(preset) || !nzchar(preset)) "reference" else preset
    if (identical(preset, "off")) {
      return(list(enabled = FALSE, scenario = "off", scenario_label = "Off", rate = 0, targets = 30, calibration = "conservative", max_multiplier = 1.0, adaptive_saturation = FALSE, saturation_exponent = 1.0, interpretation = "Off: no dynamic replacement module."))
    }
    if (identical(preset, "low")) {
      return(list(enabled = TRUE, scenario = "low", scenario_label = "Low opportunity pressure", rate = 1e-7, targets = 20, calibration = "conservative", max_multiplier = 1.10, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "Low: expected to generate few or no established replacement events in many runs; adaptive-space saturation is active."))
    }
    if (identical(preset, "high")) {
      return(list(enabled = TRUE, scenario = "high", scenario_label = "High opportunity pressure", rate = 2e-6, targets = 50, calibration = "high", max_multiplier = 1.65, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "High: stress-test scenario with more population-level opportunities; adaptive-space saturation limits marginal gains near the cap."))
    }
    list(enabled = TRUE, scenario = "reference", scenario_label = "Reference opportunity pressure", rate = 5e-7, targets = 30, calibration = "reference", max_multiplier = 1.35, adaptive_saturation = TRUE, saturation_exponent = 1.0, interpretation = "Reference: exploratory moderate population-level opportunity pressure; adaptive-space saturation is active.")
  }

  get_dynamic_config <- function() {
    if (identical(isolate(input$active_scenario_mode), "guided")) {
      return(dynamic_preset_values(isolate(input$dynamic_module_scenario)))
    }
    enabled <- !identical(isolate(input$evolution_module_mode), "off")
    list(
      enabled = enabled,
      scenario = if (enabled) "custom_advanced" else "off",
      scenario_label = if (enabled) "Custom advanced" else "Off",
      rate = if (enabled) isolate(input$mutation_rate_per_replication) else 0,
      targets = isolate(input$effective_mutation_targets),
      calibration = isolate(input$variant_emergence_calibration),
      max_multiplier = if (enabled) isolate(input$maximum_variant_R0_multiplier) else 1.0,
      adaptive_saturation = enabled,
      saturation_exponent = 1.0
    )
  }




  summarize_dynamic_robustness <- function(results) {
    if (is.null(results) || nrow(results) == 0) return(data.frame(Message = "No robustness results available."))
    safe_mean <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!any(is.finite(x))) return(NA_real_)
      mean(x[is.finite(x)])
    }
    safe_quantile <- function(x, prob) {
      x <- suppressWarnings(as.numeric(x))
      if (!any(is.finite(x))) return(NA_real_)
      as.numeric(stats::quantile(x[is.finite(x)], prob, na.rm = TRUE))
    }
    scenarios <- split(results, results$dynamic_scenario)
    rows <- lapply(names(scenarios), function(sc) {
      x <- scenarios[[sc]]
      data.frame(
        dynamic_scenario = sc,
        replicates = nrow(x),
        mean_candidate_events = round(safe_mean(x$dynamic_candidate_events), 2),
        mean_established_events = round(safe_mean(x$dynamic_established_events), 2),
        mean_final_effective_R0 = round(safe_mean(x$dynamic_final_effective_R0), 4),
        p05_final_effective_R0 = round(safe_quantile(x$dynamic_final_effective_R0, 0.05), 4),
        p95_final_effective_R0 = round(safe_quantile(x$dynamic_final_effective_R0, 0.95), 4),
        mean_peak_active = round(mean(x$peak_active, na.rm = TRUE)),
        mean_peak_day = round(mean(x$peak_day, na.rm = TRUE), 1),
        mean_final_deaths = round(mean(x$final_deaths, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
    })
    summary <- do.call(rbind, rows)
    ord <- match(summary$dynamic_scenario, c("low", "reference", "high"))
    summary <- summary[order(ord), , drop = FALSE]

    summary
  }

  summarize_dynamic_robustness_global <- function(results) {
    if (is.null(results) || nrow(results) == 0) {
      return(data.frame(
        Metric = "global_dynamic_robustness_comparison",
        Value = "not_available",
        stringsAsFactors = FALSE
      ))
    }
    safe_mean <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!any(is.finite(x))) return(NA_real_)
      mean(x[is.finite(x)])
    }
    ref <- results[results$dynamic_scenario == "reference", , drop = FALSE]
    high <- results[results$dynamic_scenario == "high", , drop = FALSE]
    if (nrow(ref) == 0 || nrow(high) == 0) {
      return(data.frame(
        Metric = c("global_high_gt_reference_probability", "global_high_minus_reference_mean_R0", "global_dynamic_preset_interpretation"),
        Value = c(NA, NA, "reference_or_high_results_missing"),
        stringsAsFactors = FALSE
      ))
    }
    n <- min(nrow(ref), nrow(high))
    ref <- ref[seq_len(n), , drop = FALSE]
    high <- high[seq_len(n), , drop = FALSE]
    ref_r0 <- suppressWarnings(as.numeric(ref$dynamic_final_effective_R0))
    high_r0 <- suppressWarnings(as.numeric(high$dynamic_final_effective_R0))
    valid <- is.finite(ref_r0) & is.finite(high_r0)
    p_high_gt_ref <- if (any(valid)) mean(high_r0[valid] > ref_r0[valid]) else NA_real_
    diff_mean <- if (any(valid)) mean(high_r0[valid] - ref_r0[valid]) else NA_real_
    ref_mean <- safe_mean(ref_r0)
    high_mean <- safe_mean(high_r0)
    interpretation <- if (!is.finite(p_high_gt_ref)) {
      "insufficient_valid_pairs"
    } else if (p_high_gt_ref >= 0.75) {
      "strong_average_separation_high_above_reference"
    } else if (p_high_gt_ref >= 0.60) {
      "high_pressure_higher_on_average_but_not_deterministic"
    } else if (p_high_gt_ref >= 0.55) {
      "modest_average_separation_between_reference_and_high"
    } else {
      "weak_separation_between_reference_and_high"
    }
    data.frame(
      Metric = c(
        "global_reference_mean_final_R0",
        "global_high_mean_final_R0",
        "global_high_gt_reference_probability",
        "global_high_minus_reference_mean_R0",
        "global_dynamic_preset_interpretation"
      ),
      Value = c(
        ifelse(is.finite(ref_mean), sprintf("%.4f", ref_mean), NA),
        ifelse(is.finite(high_mean), sprintf("%.4f", high_mean), NA),
        ifelse(is.finite(p_high_gt_ref), sprintf("%.3f", p_high_gt_ref), NA),
        ifelse(is.finite(diff_mean), sprintf("%.4f", diff_mean), NA),
        interpretation
      ),
      stringsAsFactors = FALSE
    )
  }

  observeEvent(input$run_dynamic_robustness, {
    reps <- max(3, min(50, as.integer(input$dynamic_robustness_replicates)))
    guided_mode <- identical(isolate(input$active_scenario_mode), "guided")
    base_seed <- if (guided_mode) guided_internal_seed() else parse_optional_seed(isolate(input$stochastic_seed))
    if (is.null(base_seed)) base_seed <- 12345L
    scenarios <- c("low", "reference", "high")

    # Resolve the starting country locally for the robustness observer.
    # This intentionally mirrors the main run button logic, but avoids relying
    # on a helper that may not exist in older intermediate app versions.
    starting_country_mode <- if (guided_mode) "manual" else isolate(input$starting_country_mode)
    current_country <- isolate(input$starting_country)
    if (identical(starting_country_mode, "manual")) {
      start_country <- current_country
    } else if (identical(starting_country_mode, "seeded_stable") && !is.null(base_seed)) {
      set.seed(base_seed)
      start_country <- sample(COUNTRIES_LIST, 1)
    } else if (identical(starting_country_mode, "seeded_stable") && is.null(base_seed)) {
      start_country <- current_country
    } else {
      available_countries <- setdiff(COUNTRIES_LIST, current_country)
      if (length(available_countries) == 0) available_countries <- COUNTRIES_LIST
      set.seed(base_seed + 7919L)
      start_country <- sample(available_countries, 1)
    }
    updateSelectInput(session, "starting_country", selected = start_country)

    containment_schedule <- make_containment_schedule(
      enabled = ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))),
      preset = isolate(input$containment_preset),
      start_day = isolate(input$containment_start_day),
      end_day = isolate(input$containment_end_day),
      custom_transmission_reduction = isolate(input$containment_transmission_reduction),
      custom_mobility_reduction = isolate(input$containment_mobility_reduction),
      label = isolate(input$containment_label),
      geographic_scope = isolate(input$containment_geographic_scope),
      affected_continents = isolate(input$containment_affected_continents),
      affected_countries = isolate(input$containment_affected_countries)
    )
    rows <- list()
    k <- 0
    withProgress(message = "Dynamic robustness check", value = 0, {
      total <- length(scenarios) * reps
      for (sc in scenarios) {
        cfg <- dynamic_preset_values(sc)
        for (r in seq_len(reps)) {
          k <- k + 1
          incProgress(1 / total, detail = paste(sc, "replicate", r, "of", reps))
          seed <- as.integer(base_seed + r - 1)
          res <- run_simulation_age_adjusted(
            R0_target = isolate(input$R0),
            infectious_period_days = isolate(input$infectious_period_days),
            mortality_rate = isolate(input$mortality_rate) / 100,
            N = WORLD_POPULATION,
            I0 = isolate(input$initial_infected),
            days = isolate(input$simulation_days),
            mutation_rate_per_replication = cfg$rate,
            effective_mutation_targets = cfg$targets,
            variant_emergence_calibration = cfg$calibration,
            maximum_variant_R0_multiplier = cfg$max_multiplier,
            enable_evolution = isTRUE(cfg$enabled),
            starting_country = start_country,
            air_travel_scenario = isolate(input$air_travel_scenario),
            import_establishment_probability = isolate(input$import_establishment_probability),
            infectiousness_profile = isolate(input$infectiousness_profile),
            rng_seed = seed,
            progress_callback = NULL,
            neutral_age_weights = FALSE,
            age_cfr_scale = isolate(input$age_cfr_scale),
            containment_schedule = containment_schedule,
            dynamic_adaptive_saturation = isTRUE(cfg$adaptive_saturation),
            dynamic_saturation_exponent = cfg$saturation_exponent,
            age_distribution_mode = if (!is.null(isolate(input$age_distribution_mode))) isolate(input$age_distribution_mode) else "country_specific"
          )
          d <- res$data
          peak_idx <- which.max(d$I)
          cp <- res$cost_profile
          if (is.null(cp)) cp <- list()

          # The lower-level simulation engine returns the raw dynamic objects,
          # while the main Run button later augments cost_profile with formatted
          # dynamic fields. The robustness observer calls the engine directly, so
          # dynamic metrics must be computed here from the raw result.
          dyn_diag <- calculate_dynamic_diagnostics(res, isolate(input$R0), cfg$max_multiplier)
          dyn_value <- function(metric, numeric = TRUE) {
            if (is.null(dyn_diag) || nrow(dyn_diag) == 0 || !metric %in% dyn_diag$Metric) return(NA_real_)
            v <- dyn_diag$Value[dyn_diag$Metric == metric][1]
            if (numeric) return(suppressWarnings(as.numeric(v)))
            v
          }
          candidate_events <- if (!is.null(res$candidate_mutations) && is.data.frame(res$candidate_mutations)) nrow(res$candidate_mutations) else 0
          established_events <- if (!is.null(res$variants_emerged) && is.data.frame(res$variants_emerged) && "variant_id" %in% names(res$variants_emerged)) {
            sum(res$variants_emerged$variant_id != 1, na.rm = TRUE)
          } else 0
          final_effective_R0 <- suppressWarnings(as.numeric(res$final_R0))
          if (!is.finite(final_effective_R0)) final_effective_R0 <- dyn_value("dynamic_dominant_variant_R0_final")

          rows[[length(rows) + 1]] <- data.frame(
            dynamic_scenario = sc,
            replicate = r,
            seed = seed,
            dynamic_candidate_events = candidate_events,
            dynamic_established_events = established_events,
            dynamic_final_effective_R0 = final_effective_R0,
            dynamic_max_established_variant_R0 = dyn_value("dynamic_max_established_variant_R0"),
            dynamic_dominant_variant_share_final = dyn_value("dynamic_dominant_variant_share_final"),
            peak_active = round(d$I[peak_idx]),
            peak_day = d$time[peak_idx],
            final_deaths = round(tail(d$D, 1)),
            elapsed_seconds = ifelse(is.null(cp$elapsed_seconds), NA, as.numeric(cp$elapsed_seconds)),
            stringsAsFactors = FALSE
          )
        }
      }
    })
    dynamic_robustness_results(do.call(rbind, rows))
  })

  output$dynamic_robustness_table <- renderTable({
    res <- dynamic_robustness_results()
    if (is.null(res)) return(data.frame(Message = "Run the dynamic robustness check to evaluate presets across seeds."))
    summarize_dynamic_robustness(res)
  }, rownames = FALSE)

  output$dynamic_robustness_global_table <- renderTable({
    res <- dynamic_robustness_results()
    if (is.null(res)) return(data.frame(Message = "Run the dynamic robustness check to evaluate presets across seeds."))
    summarize_dynamic_robustness_global(res)
  }, rownames = FALSE)

  output$dynamic_robustness_report <- renderText({
    res <- dynamic_robustness_results()
    if (is.null(res)) return("DYNAMIC_ROBUSTNESS_REPORT_NOT_RUN")
    summary <- summarize_dynamic_robustness(res)
    global <- summarize_dynamic_robustness_global(res)
    lines <- c("DYNAMIC_ROBUSTNESS_REPORT_START")
    lines <- c(lines, paste0("replicates_per_preset=", unique(summary$replicates)[1]))
    for (i in seq_len(nrow(summary))) {
      sc <- summary$dynamic_scenario[i]
      for (nm in names(summary)) {
        lines <- c(lines, paste0(sc, "_", nm, "=", summary[[nm]][i]))
      }
    }
    lines <- c(lines, "GLOBAL_DYNAMIC_COMPARISON_START")
    for (i in seq_len(nrow(global))) {
      lines <- c(lines, paste0(global$Metric[i], "=", global$Value[i]))
    }
    lines <- c(lines, "GLOBAL_DYNAMIC_COMPARISON_END")
    lines <- c(lines, "interpretation=presets_are_stochastic_evaluate_in_distribution_not_single_seed", "DYNAMIC_ROBUSTNESS_REPORT_END")
    paste(lines, collapse = "\n")
  })

  observeEvent(input$play_animation, {
    animation_running(TRUE)
    leafletProxy("world_map_infected") %>% clearControls()
    leafletProxy("world_map_deaths") %>% clearControls()
  })
  observeEvent(input$pause_animation, {
    animation_running(FALSE)
  })

  observe({
    req(animation_running())
    invalidateLater(100, session)
    current_day <- isolate(input$time_slider)
    max_day <- isolate(input$simulation_days)
    if (current_day < max_day) {
      updateSliderInput(session, "time_slider", value = current_day + 1)
    } else {
      animation_running(FALSE)
    }
  })

  observeEvent(input$random_start_country, {
    current_country <- isolate(input$starting_country)
    available_countries <- setdiff(COUNTRIES_LIST, current_country)
    if (length(available_countries) == 0) available_countries <- COUNTRIES_LIST

    guided_mode <- identical(isolate(input$active_scenario_mode), "guided")
    if (guided_mode) {
      # In basic guided mode the seed is hidden from the user but deterministic
      # within the session and stored for future JSON export.
      click_count <- as.integer(isolate(input$random_start_country))
      guided_random_country_clicks(click_count)
      set.seed(as.integer(guided_internal_seed()) + click_count * 7919L)
      selected_country <- sample(available_countries, 1)
      guided_country_selection_source("basic_random_button")
    } else {
      optional_seed <- parse_optional_seed(isolate(input$stochastic_seed))
      if (!is.null(optional_seed)) {
        set.seed(optional_seed + isolate(input$random_start_country) * 7919L)
        selected_country <- sample(COUNTRIES_LIST, 1)
      } else {
        selected_country <- sample(available_countries, 1)
      }
      guided_country_selection_source("advanced_or_manual")
    }
    updateSelectInput(session, "starting_country", selected = selected_country)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_covid_values, {
    updateNumericInput(session, "R0", value = 4.25)
    updateNumericInput(session, "infectious_period_days", value = 5)
    updateNumericInput(session, "exposed_period_days", value = 4)
    updateSelectInput(session, "infectiousness_profile", selected = "early")
    updateNumericInput(session, "mortality_rate", value = 1)
    updateNumericInput(session, "mutation_rate_per_replication", value = 5e-7)
    updateNumericInput(session, "effective_mutation_targets", value = 30)
    updateSelectInput(session, "variant_emergence_calibration", selected = "conservative")
    updateNumericInput(session, "maximum_variant_R0_multiplier", value = 1.0)
    updateSelectInput(session, "starting_country_mode", selected = "manual")
    updateSelectInput(session, "starting_country", selected = "South Africa")
    updateSelectInput(session, "air_travel_scenario", selected = "reference")
    updateNumericInput(session, "import_establishment_probability", value = 1.00)
    updateNumericInput(session, "simulation_days", value = 365)
    updateSelectInput(session, "evolution_module_mode", selected = "off")
    updateSelectInput(session, "dynamic_module_scenario", selected = "off")
    guided_country_selection_source("manual_or_default")
    guided_random_country_clicks(0L)
  })

  observeEvent(input$reset_defaults, {
    updateSelectInput(session, "active_scenario_mode", selected = "guided")
    updateCheckboxInput(session, "show_technical_validation_options", value = FALSE)
    updateSelectInput(session, "model_structure", selected = "age_adjusted")
    updateSelectInput(session, "age_parameter_mode", selected = "differential_cfr")
    updateNumericInput(session, "age_cfr_scale", value = 1)
    updateNumericInput(session, "R0", value = 5.0)
    updateNumericInput(session, "infectious_period_days", value = 20)
    updateSelectInput(session, "infectiousness_profile", selected = "mid")
    updateNumericInput(session, "mortality_rate", value = 1.0)
    updateSelectInput(session, "evolution_module_mode", selected = "active")
    updateSelectInput(session, "dynamic_module_scenario", selected = "reference")
    updateNumericInput(session, "mutation_rate_per_replication", value = 5e-7)
    updateNumericInput(session, "effective_mutation_targets", value = 30)
    updateSelectInput(session, "variant_emergence_calibration", selected = "reference")
    updateNumericInput(session, "maximum_variant_R0_multiplier", value = 1.35)
    updateSelectInput(session, "starting_country_mode", selected = "seeded_stable")
    updateSelectInput(session, "starting_country", selected = sample(COUNTRIES_LIST, 1))
    updateNumericInput(session, "initial_infected", value = 5)
    updateSelectInput(session, "air_travel_scenario", selected = "reference")
    updateNumericInput(session, "import_establishment_probability", value = 1.00)
    updateTextInput(session, "stochastic_seed", value = "")
    updateNumericInput(session, "simulation_days", value = 365)
    updateRadioButtons(session, "global_plot_scale", selected = "persons")
    updateCheckboxInput(session, "enable_calibration_targets", value = TRUE)
    updateNumericInput(session, "calibration_population", value = WORLD_POPULATION)
    updateNumericInput(session, "observed_positive_cumulative", value = NA)
    updateNumericInput(session, "case_detection_fraction", value = 0.20)
    updateNumericInput(session, "observed_positive_day", value = 60)
    updateNumericInput(session, "positive_lag_days", value = 5)
    updateNumericInput(session, "observed_seroprevalence_percent", value = NA)
    updateNumericInput(session, "observed_seroprevalence_day", value = 90)
    updateNumericInput(session, "seroprevalence_lag_days", value = 26)
  })

  output$covid_reference_card <- renderUI({
    ref_file <- tryCatch(basename(selected_fixed_reference_file()), error = function(e) "fixed_covid_omicron_reference_sir.rds")
    ref_type <- tryCatch(selected_fixed_reference_type(), error = function(e) "basic")
    fallback_note <- tryCatch(selected_fixed_reference_fallback_note(), error = function(e) "")

    ref_cache <- tryCatch(selected_fixed_reference_cache(), error = function(e) NULL)
    ref_model <- if (!is.null(ref_cache$compartment_model)) ref_cache$compartment_model else ifelse(identical(ref_type, "age_adjusted_seird"), "SEIRD", "SIRD")
    ref_exposed_period <- if (!is.null(ref_cache$exposed_period_days)) ref_cache$exposed_period_days else ifelse(identical(ref_model, "SEIRD"), 4, 0)
    ref_peak_exposed <- NA
    ref_peak_exposed_day <- NA
    if (!is.null(ref_cache$data) && "E" %in% names(ref_cache$data)) {
      eidx <- which.max(ref_cache$data$E)
      ref_peak_exposed <- format(round(ref_cache$data$E[eidx]), big.mark = ",")
      ref_peak_exposed_day <- ref_cache$data$time[eidx]
    } else if (!is.null(ref_cache$exposed_country_history)) {
      exposed_total <- rowSums(ref_cache$exposed_country_history, na.rm = TRUE)
      eidx <- which.max(exposed_total)
      ref_peak_exposed <- format(round(exposed_total[eidx]), big.mark = ",")
      ref_peak_exposed_day <- eidx - 1
    }

    comparator_items <- list(
      c("Type", dplyr::case_when(
        identical(ref_type, "age_adjusted_seird") ~ "Age-adjusted Omicron SEIRD RDS",
        identical(ref_type, "age_adjusted_sird") ~ "Age-adjusted Omicron SIRD RDS",
        identical(ref_type, "age_adjusted") ~ "Age-adjusted Omicron SIRD RDS",
        TRUE ~ "Basic Omicron SIRD RDS"
      )),
      c("Modelo", ref_model),
      c("E: exposed phase", ifelse(identical(ref_model, "SEIRD"), paste0(ref_exposed_period, " days to active phase"), "Not applicable in SIRD")),
      c("Peak E", ifelse(is.na(ref_peak_exposed), "Not available", paste0(ref_peak_exposed, " persons; day ", ref_peak_exposed_day))),
      c("Reference", "COVID-19 Omicron"),
      c("Starting country", "South Africa"),
      c("R0", "4.25"),
      c("Active window", "5 days"),
      c("Engine", ifelse(grepl("age_adjusted", ref_type), "Age-adjusted differential CFR", "Basic aggregated reference"))
    )
    if (!is.null(fallback_note) && nzchar(fallback_note)) {
      comparator_items <- c(comparator_items, list(c("Fallback", fallback_note)))
    }

    tags$div(
      class = "fixed-ref-card",
      tags$div(
        class = "fixed-ref-grid",
        lapply(comparator_items, function(item) {
          tagList(
            tags$div(class = "fixed-ref-label", item[[1]]),
            tags$div(class = "fixed-ref-value", item[[2]])
          )
        })
      )
    )
  })

  build_default_assumptions_registry <- function() {
    data.frame(
      Parameter = c(
        "fixed_covid_rds",
        "active_scenario_default_engine",
        "R0_input",
        "infectious_window_days",
        "infectiousness_profile",
        "mortality_rate_default_guided",
        "age_group_structure",
        "age_group_CFR_profile",
        "case_detection_fraction",
        "positive_lag_days",
        "seroprevalence_lag_days",
        "air_travel_scenario",
        "import_establishment_probability",
        "containment_mild",
        "containment_moderate",
        "containment_strong",
        "containment_geographic_scope",
        "mutation_rate_per_replication",
        "effective_mutation_targets",
        "map_scaling",
        "animation_interval_ms"
      ),
      Current_value = c(
        "fixed_covid_omicron_reference_sir.rds",
        "age-adjusted model with differential CFR in guided mode",
        as.character(isolate(input$R0)),
        as.character(isolate(input$infectious_period_days)),
        as.character(isolate(input$infectiousness_profile)),
        paste0(as.character(isolate(input$mortality_rate)), "%"),
        "6 age groups: 0-9, 10-19, 20-39, 40-59, 60-79, 80+",
        "Levin-style age gradient scaled to weighted global target",
        as.character(isolate(input$case_detection_fraction)),
        as.character(isolate(input$positive_lag_days)),
        as.character(isolate(input$seroprevalence_lag_days)),
        as.character(isolate(input$air_travel_scenario)),
        as.character(isolate(input$import_establishment_probability)),
        "25% transmission reduction; 20% mobility reduction",
        "50% transmission reduction; 50% mobility reduction",
        "75% transmission reduction; 75% mobility reduction",
        as.character(isolate(input$containment_geographic_scope)),
        as.character(isolate(input$mutation_rate_per_replication)),
        as.character(isolate(input$effective_mutation_targets)),
        "pseudo-log marker scaling with minimum visible radius when boost is enabled",
        "100"
      ),
      Unit = c(
        "file",
        "model route",
        "dimensionless",
        "days",
        "category",
        "percent",
        "groups",
        "proportion",
        "proportion 0-1",
        "days",
        "days",
        "category",
        "multiplier",
        "percent",
        "percent",
        "percent",
        "scope",
        "per replication",
        "count",
        "visual rule",
        "milliseconds"
      ),
      Used_in = c(
        "comparator",
        "active scenario",
        "transmission",
        "state duration",
        "temporal profile",
        "age-adjusted CFR scaling",
        "age-adjusted engine",
        "age-adjusted engine",
        "calibration diagnostics",
        "calibration diagnostics",
        "calibration diagnostics",
        "mobility matrix",
        "imported-opportunity term",
        "containment preset",
        "containment preset",
        "containment preset",
        "containment module",
        "dynamic module",
        "dynamic module",
        "map visualization",
        "animation"
      ),
      Status = c(
        "technical default",
        "reviewed architecture",
        "scenario parameter",
        "reviewed/provisional",
        "reviewed/provisional",
        "reviewed/provisional",
        "provisional",
        "reviewed/provisional",
        "reviewed/provisional",
        "reviewed/provisional",
        "reviewed/provisional",
        "scenario preset",
        "scenario preset",
        "scenario preset",
        "scenario preset",
        "scenario preset",
        "reviewed architecture",
        "needs literature review",
        "needs literature review",
        "technical default",
        "technical default"
      ),
      Plausible_range_or_default_context = c(
        "not applicable",
        "not applicable",
        "scenario-specific",
        "5 days for Omicron-like; 7-10 days for early-lineage-like scenarios",
        "early/mid/flat/late as scenario kernels",
        "~0.1-1% global target for COVID-like guided scenarios; higher values are stress-test scenarios",
        "6 broad teaching/planning groups",
        "very low in young groups; steep age gradient; scaled to selected global target",
        "context dependent; app default 0.20; serology-implied value can be calculated",
        "3-7 days; default 5",
        "18-30 days; default 26",
        "scenario-specific",
        "0-1",
        "low-intensity package",
        "intermediate package",
        "high-intensity package",
        "global / continent / selected countries",
        "pending",
        "pending",
        "not applicable",
        "100-1000 ms depending on browser load"
      ),
      Source_or_review_status = c(
        "Generated outside Shiny; loaded at runtime",
        "Internal architecture decision",
        "Preset-specific parameter; user-editable",
        "Xu et al. 2023 supports shorter Omicron intervals; early-lineage defaults still provisional",
        "Temporal kernel is a modelling choice; interval evidence informs plausible windows",
        "Default reduced to 1%; age gradient contextualised with Levin et al. 2020",
        "Simplified global age structure; pending country-specific refinement",
        "Levin et al. 2020 age-specific IFR gradient; scaled in-app to selected global target",
        "ENE-COVID supports under-detection; user value can be compared with serology-implied fraction",
        "Approximate interval from underlying event to recorded positive count; supported by incubation/testing timing literature; shorter for Omicron-like scenarios",
        "Approximate interval from underlying event to detectable serological signal; IgM/IgG timing varies by assay, symptoms, severity and sampling design",
        "Scenario input; guided mode hides advanced traffic choice",
        "Scenario input; guided mode hides advanced import modifier",
        "Scenario preset; literature supports broad effect ranges rather than one fixed value",
        "Scenario preset; literature supports broad effect ranges rather than one fixed value",
        "Scenario preset; literature supports broad effect ranges rather than one fixed value",
        "Internal architecture decision",
        "Placeholder macro-parameter; pending targeted review",
        "Placeholder macro-parameter; pending targeted review",
        "Visualization choice",
        "Visualization choice"
      ),
      Reference_link = c(
        "local RDS file",
        "internal",
        "preset/user-defined",
        "https://pmc.ncbi.nlm.nih.gov/articles/PMC10541713/",
        "https://pmc.ncbi.nlm.nih.gov/articles/PMC10541713/",
        "https://link.springer.com/article/10.1007/s10654-020-00698-1",
        "pending",
        "https://link.springer.com/article/10.1007/s10654-020-00698-1",
        "https://pubmed.ncbi.nlm.nih.gov/32645347/",
        "https://www.cdc.gov/covid/hcp/clinical-care/covid19-presentation.html",
        "https://www.nature.com/articles/s41598-021-82428-5",
        "https://www.google.com/covid19/mobility/",
        "scenario/user-defined",
        "scenario preset; see containment literature notes",
        "scenario preset; see containment literature notes",
        "scenario preset; see containment literature notes",
        "internal",
        "pending",
        "pending",
        "internal",
        "internal"
      ),
      Comment = c(
        "The comparator is not recalculated when running the active scenario.",
        "Basic and neutral engines remain available only for technical validation.",
        "Should be preset-specific in final guided mode.",
        "Guided value remains editable; Omicron comparator uses 5 days.",
        "Defines distribution across active-state days.",
        "Accessible in guided mode; high values are allowed for stress testing but flagged as extreme.",
        "Currently global, not country-specific.",
        "Levin values provide context; the app scales the profile to the selected global target.",
        "Observed positives are compared after multiplying simulated cumulative by this fraction.",
        "Observed positives are shifted backward by this lag. This is a calibration parameter, not a fixed biological constant.",
        "Seroprevalence is shifted backward by this lag. This is a calibration parameter, not a fixed biological constant.",
        "Hidden in guided mode; visible in advanced mode.",
        "Hidden in guided mode; visible in advanced mode.",
        "Applies only to active scenario.",
        "Applies only to active scenario.",
        "Applies only to active scenario.",
        "Global, continent or selected-country scope.",
        "Currently active only where the dynamic module is enabled.",
        "Currently active only where the dynamic module is enabled.",
        "Improves early-map readability; does not change calculations.",
        "Can be revisited if browser load increases."
      ),
      stringsAsFactors = FALSE
    )
  }

  build_default_bibliography_table <- function() {
    data.frame(
      Topic = c(
        "Age-specific CFR/IFR context",
        "Temporal intervals / Omicron shorter timing",
        "Positive-record lag / incubation and testing timing",
        "IgM/IgG seroconversion timing",
        "Longer serology window context",
        "Detection fraction and serology/PCR gap",
        "Seroprevalence example context",
        "Mobility data context",
        "Containment scenario ranges",
        "Dynamic module macro-effective rate context",
        "Dynamic module R0 cap context",
        "Dynamic module target definition"
      ),
      Source = c(
        "Levin AT, Cochran KB, Walsh SP. Assessing the age specificity of infection fatality rates for COVID-19. Eur J Epidemiol. 2020.",
        "Xu X et al. Assessing changes in incubation period, serial interval, and generation time of SARS-CoV-2 variants of concern: systematic review and meta-analysis. 2023.",
        "CDC clinical presentation summary and variant timing literature. Omicron incubation is shorter than early-lineage SARS-CoV-2; earlier estimates commonly centred near 5 days.",
        "Nakano Y et al. Time course of anti-SARS-CoV-2 antibody sensitivity/specificity after symptom onset; median IgM/IgG seroconversion often 10 days or longer after symptom onset.",
        "Long-term seroprevalence literature reports IgM/IgG increasing markedly by about 2 weeks after symptom onset, while timing varies across assays and populations.",
        "Pollan M et al. Prevalence of SARS-CoV-2 in Spain (ENE-COVID): nationwide population-based seroepidemiological study. Lancet. 2020.",
        "ENE-COVID public communications and article context for early Spanish seroprevalence around 5%, with territorial variation.",
        "Google COVID-19 Community Mobility Reports: historical aggregated mobility trends, no longer updated after 2022-10-15.",
        "Published policy-effect literature reports heterogeneous reductions; app presets are scenario levels, not universal estimates.",
        "Amicone et al. estimate SARS-CoV-2 mutation-rate order of magnitude around 1.3e-6 per base per infection cycle; the app uses this only as contextual scale, not as a direct per-base model parameter.",
        "Omicron R0/Re estimates and variant-comparison literature show substantial relative changes in population Rt/R0; the app cap is frequency-weighted and not cumulative.",
        "Internal model definition: population-level replacement opportunity multiplier are abstract transmission-relevant opportunities used to scale candidate generation; they are not nucleotide sites or genome length."
      ),
      Link = c(
        "https://link.springer.com/article/10.1007/s10654-020-00698-1",
        "https://pmc.ncbi.nlm.nih.gov/articles/PMC10541713/",
        "https://www.cdc.gov/covid/hcp/clinical-care/covid19-presentation.html",
        "https://www.nature.com/articles/s41598-021-82428-5",
        "https://link.springer.com/article/10.1186/s12879-023-08425-3",
        "https://pubmed.ncbi.nlm.nih.gov/32645347/",
        "https://www.lamoncloa.gob.es/lang/en/gobierno/news/Paginas/2020/06072020_serostudy-.aspx",
        "https://www.google.com/covid19/mobility/",
        "https://www.nature.com/articles/s41598-021-81442-x",
        "https://pmc.ncbi.nlm.nih.gov/articles/PMC8996265/",
        "https://pmc.ncbi.nlm.nih.gov/articles/PMC8992231/",
        "Internal model definition"
      ),
      How_used_in_app = c(
        "Provides age-gradient context; guided global mortality default lowered to 1%, with age-specific scaling visible in the assumptions tab.",
        "Supports shorter Omicron timing and informs active-window/profile documentation.",
        "Supports keeping positive_lag_days as a calibration default of 5 days with plausible range 3-7 days.",
        "Supports seroprevalence_lag_days as a calibration default of 26 days with plausible range 18-30 days.",
        "Used to document that serology lag is an approximate calibration parameter, not a universal constant.",
        "Supports explicit detection-fraction diagnostics and under-detection explanation.",
        "Provides example seroprevalence context; not a universal default.",
        "Supports separation of mobility reduction from transmission reduction.",
        "Supports treating mild/moderate/strong as scenario presets over a broad plausible range.",
        "Supports order-of-magnitude context for the macro-effective opportunity rate; values remain scenario presets requiring calibration.",
        "Supports a guided high cap of 1.65 as a plausible stress-test bound rather than 2.0; advanced mode can still explore higher caps.",
        "Defines targets as an abstract scaling factor for candidate generation, not a biological sequence parameter."
      ),
      stringsAsFactors = FALSE
    )
  }


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
        ifelse(exists("COUNTRY_AGE_DISTRIBUTION", inherits = TRUE) && !is.null(COUNTRY_AGE_DISTRIBUTION$data), "available", "fallback or missing"),
        ifelse(world_polygon_available(), "available", "fallback markers only"),
        ifelse(identical(selected_fixed_reference_type(), "age_adjusted_seird"), "available / selected", "not selected or missing"),
        "ok",
        "ok",
        "ok",
        "session-level"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$default_assumptions_registry <- renderTable({
    build_default_assumptions_registry()
  }, rownames = FALSE)

  output$default_bibliography_table <- renderTable({
    dplyr::bind_rows(
      build_default_bibliography_table(),
      data.frame(
        Topic = c("SEIRD exposed-compartment default", "Omicron latent/incubation timing"),
        Source = c(
          "Liu Y et al. Estimating the incubation period of SARS-CoV-2 Omicron BA.1 cases in South Korea. 2022.",
          "Xin H et al. Transmission dynamics of SARS-CoV-2 Omicron variant infections. 2023."
        ),
        Link = c(
          "https://pmc.ncbi.nlm.nih.gov/articles/PMC9349028/",
          "https://www.sciencedirect.com/science/article/pii/S1201971222005719"
        ),
        How_used_in_app = c(
          "Supports that Omicron incubation is shorter than ancestral SARS-CoV-2; the app uses 4 days as an Omicron-like exposure-to-active default.",
          "Reports mean incubation around 3.8 days and latent period around 3.1 days in an Omicron outbreak; supports 4 days as a conservative rounded teaching/planning default."
        ),
        stringsAsFactors = FALSE
      )
    )
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
    cfg <- get_dynamic_config()
    data.frame(
      Parameter = c("Dynamic scenario", "Enabled", "Macro-effective opportunity rate", "Population-level replacement opportunity multiplier", "Calibration level", "Maximum R0 multiplier", "Preset interpretation", "Targets definition"),
      Value = c(cfg$scenario_label, as.character(cfg$enabled), as.character(cfg$rate), as.character(cfg$targets), as.character(cfg$calibration), as.character(cfg$max_multiplier), ifelse(is.null(cfg$interpretation), "Population-level replacement dynamics; reviewed conservative guided preset", cfg$interpretation), "Abstract count of transmission-relevant macro opportunities used to scale candidate generation; not a genomic site count"),
      Status = c("scenario preset", "scenario preset", "scenario preset / needs calibration", "scenario preset / needs calibration", "scenario preset / needs calibration", "scenario cap / literature-informed", "scope statement", "definition"),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)


  output$reference_comparator_metadata_table <- renderTable({
    ref_file <- tryCatch(selected_fixed_reference_file(), error = function(e) NA_character_)
    ref_type <- tryCatch(selected_fixed_reference_type(), error = function(e) "basic")
    ref_cache <- tryCatch(selected_fixed_reference_cache(), error = function(e) NULL)
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
        ifelse(is.na(ref_file), "not available", basename(ref_file)),
        ref_model,
        ifelse(identical(ref_model, "SEIRD"), paste0("Included; ", ref_exposed, " days from exposure to active phase"), "Not applicable in SIRD"),
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

  output$logic_availability_table <- renderTable({
    data.frame(
      Component = c(
        "COVID comparator",
        "Initial country selection",
        "Core scenario controls",
        "Containment presets and scope",
        "World maps and main outputs",
        "Random seed",
        "Calibration inputs",
        "Passenger traffic and import settings",
        "Dynamic robustness diagnostics",
        "Technical validation options"
      ),
      Guided_basic_mode = c(
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Hidden / stored internally",
        "No",
        "No",
        "No",
        "No"
      ),
      Complete_advanced_mode = c(
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes",
        "Yes"
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$model_scope_report <- renderText({
    reg <- build_default_assumptions_registry()
    status_counts <- table(reg$Status)
    status_line <- paste(paste(names(status_counts), as.integer(status_counts), sep = "="), collapse = ";")
    lines <- c(
      "MODEL_SCOPE_REPORT_START",
      "tool_type=macroscopic_population_epidemiology_simulator",
      "primary_use=teaching_planning_visualization_scenario_exploration",
      "fixed_comparator=fixed_covid_omicron_reference_sir.rds",
      "fixed_comparator_recomputed_on_run=FALSE",
      paste0("active_scenario_mode=", isolate(input$active_scenario_mode)),
      paste0("guided_mode_default_engine=age_adjusted_differential_cfr"),
      paste0("technical_validation_visible=", isTRUE(isolate(input$show_technical_validation_options))),
      "does=aggregate_SIRD_dynamics;geographic_spread;age_group_CFR;containment_scenarios;calibration_diagnostics;maps;cost_profile",
      "does_not=predict_future_real_counts;replace_official_models;infer_policy_causality;use_individual_contacts;auto_optimize_parameters;reconstruct_history_exactly",
      paste0("assumption_status_counts=", status_line),
      paste0("bibliography_sources_listed=", nrow(build_default_bibliography_table())),
      "MODEL_SCOPE_REPORT_END"
    )
    paste(lines, collapse = "\n")
  })


  output$age_parameter_table <- renderTable({
    mortality_rate <- if (!is.null(input$mortality_rate)) input$mortality_rate / 100 else 0.01
    neutral_mode <- !identical(input$age_parameter_mode, "differential_cfr")
    cfr_scale <- if (!is.null(input$age_cfr_scale)) input$age_cfr_scale else 1
    age_params <- make_age_group_parameters(mortality_rate = mortality_rate, neutral = neutral_mode, cfr_scale = cfr_scale)
    mode <- if (!is.null(input$age_distribution_mode)) input$age_distribution_mode else "country_specific"
    country_age_distribution <- make_country_age_distribution(COUNTRIES_LIST, mode = mode)
    params <- COUNTRY_TRAVEL_PARAMS[match(COUNTRIES_LIST, COUNTRY_TRAVEL_PARAMS$country), ]
    country_populations <- setNames(params$population_millions / sum(params$population_millions) * WORLD_POPULATION, COUNTRIES_LIST)
    model_age_share <- model_age_share_from_country_distribution(country_populations, country_age_distribution)
    age_params <- update_age_params_for_model_age_share(age_params, model_age_share, mortality_rate, neutral_mode, cfr_scale)
    format_age_parameters_for_display(age_params)
  }, rownames = FALSE)



  output$country_age_distribution_table <- renderTable({
    mode <- if (!is.null(input$age_distribution_mode)) input$age_distribution_mode else "country_specific"
    dist <- make_country_age_distribution(COUNTRIES_LIST, mode = mode)
    format_country_age_distribution_for_display(dist, max_rows = 60)
  }, rownames = FALSE)

  output$age_summary_table <- renderTable({
    req(sim_data$hantavirus)
    if (is.null(sim_data$hantavirus$age_summary)) {
      return(data.frame(Message = "Age-group summary is available after running the age-group adjusted model."))
    }
    out <- sim_data$hantavirus$age_summary
    out$population <- round(out$population)
    out$active_final <- round(out$active_final)
    out$cumulative_resolved <- round(out$cumulative_resolved)
    out$deaths <- round(out$deaths)
    out$cfr <- sprintf("%.3f%%", 100 * out$cfr)
    out$death_share <- sprintf("%.1f%%", 100 * out$death_share)
    names(out) <- c("Age group", "Population", "Final active", "Resolved", "Deaths", "CFR", "Share of deaths")
    out
  }, rownames = FALSE)

  output$cost_profile_table <- renderTable({
    req(sim_data$hantavirus)
    cp <- sim_data$hantavirus$cost_profile
    if (is.null(cp)) return(data.frame(Message = "Cost profile not available yet."))
    scalarize_cost_value <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      if (length(x) > 1) return(paste(as.character(x), collapse = ";"))
      as.character(x)
    }
    data.frame(
      Metric = names(cp),
      Value = vapply(cp, scalarize_cost_value, character(1)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }, rownames = FALSE)




  calculate_calibration_diagnostics <- reactive({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    d <- h$data
    if (is.null(d) || nrow(d) == 0) return(NULL)

    cumulative_sim <- d$I + d$R + d$D
    max_day <- max(d$time, na.rm = TRUE)

    get_sim_value <- function(day_value) {
      day_value <- max(0, min(max_day, as.integer(round(day_value))))
      row_idx <- which(d$time == day_value)
      if (length(row_idx) == 0) row_idx <- which.min(abs(d$time - day_value))
      list(day = d$time[row_idx[1]], value = cumulative_sim[row_idx[1]])
    }

    pop_ref <- suppressWarnings(as.numeric(input$calibration_population))
    if (is.na(pop_ref) || pop_ref <= 0) pop_ref <- WORLD_POPULATION

    pos_obs <- suppressWarnings(as.numeric(input$observed_positive_cumulative))
    detection_fraction <- suppressWarnings(as.numeric(input$case_detection_fraction))
    if (is.na(detection_fraction) || detection_fraction <= 0) detection_fraction <- NA_real_
    if (!is.na(detection_fraction)) detection_fraction <- min(1, max(0.0001, detection_fraction))
    pos_day <- suppressWarnings(as.numeric(input$observed_positive_day))
    pos_lag <- suppressWarnings(as.numeric(input$positive_lag_days))
    if (is.na(pos_day)) pos_day <- NA
    if (is.na(pos_lag)) pos_lag <- 5

    sero_pct <- suppressWarnings(as.numeric(input$observed_seroprevalence_percent))
    sero_day <- suppressWarnings(as.numeric(input$observed_seroprevalence_day))
    sero_lag <- suppressWarnings(as.numeric(input$seroprevalence_lag_days))
    if (is.na(sero_day)) sero_day <- NA
    if (is.na(sero_lag)) sero_lag <- 26

    estimate_day0 <- isTRUE(input$enable_day0_date_estimation)
    parse_date_input <- function(x) {
      if (!estimate_day0 || is.null(x) || length(x) == 0 || is.na(x)) return(as.Date(NA))
      as.Date(x)
    }
    pos_calendar_date <- parse_date_input(input$observed_positive_calendar_date)
    sero_calendar_date <- parse_date_input(input$observed_seroprevalence_calendar_date)
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

    out <- data.frame(
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
      simulated_comparable_value = c(ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction), sero_sim$value),
      simulated_to_observed_ratio = c(safe_ratio(ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction), pos_obs), safe_ratio(sero_sim$value, sero_estimated_count)),
      relative_error = c(safe_ratio(ifelse(is.na(detection_fraction) || is.na(pos_sim$value), NA_real_, pos_sim$value * detection_fraction), pos_obs) - 1, safe_ratio(sero_sim$value, sero_estimated_count) - 1),
      observation_calendar_date = as.character(c(pos_calendar_date, sero_calendar_date)),
      estimated_event_calendar_date = as.character(c(estimate_event_date(pos_calendar_date, pos_lag), estimate_event_date(sero_calendar_date, sero_lag))),
      estimated_simulated_day0_date = as.character(c(estimate_day0_date(pos_calendar_date, pos_day), estimate_day0_date(sero_calendar_date, sero_day))),
      stringsAsFactors = FALSE
    )
    out
  })


  calculate_detection_fraction_diagnostics <- reactive({
    if (!isTRUE(input$enable_calibration_targets)) return(NULL)
    diag <- calculate_calibration_diagnostics()
    if (is.null(diag)) return(NULL)

    pos_row <- diag[diag$target == "observed_positives", , drop = FALSE]
    sero_row <- diag[diag$target == "observed_seroprevalence", , drop = FALSE]

    user_fraction <- suppressWarnings(as.numeric(input$case_detection_fraction))
    if (is.na(user_fraction) || user_fraction <= 0) user_fraction <- NA_real_
    if (!is.na(user_fraction)) user_fraction <- min(1, max(0.0001, user_fraction))

    pos_obs <- if (nrow(pos_row) == 1) suppressWarnings(as.numeric(pos_row$observed_value)) else NA_real_
    sero_estimated_count <- if (nrow(sero_row) == 1) suppressWarnings(as.numeric(sero_row$observed_value)) else NA_real_

    implied_fraction <- if (isTRUE(input$estimate_detection_from_serology) && !is.na(pos_obs) && !is.na(sero_estimated_count) && sero_estimated_count > 0) {
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
  })

  output$calibration_diagnostic_table <- renderTable({
    if (!isTRUE(input$enable_calibration_targets)) {
      return(data.frame(Message = "Calibration diagnostics are disabled."))
    }
    diag <- calculate_calibration_diagnostics()
    if (is.null(diag)) return(data.frame(Message = "Run the simulation first."))
    display <- diag
    display$observed_value <- ifelse(is.na(display$observed_value), NA, round(display$observed_value))
    display$simulated_cumulative <- ifelse(is.na(display$simulated_cumulative), NA, round(display$simulated_cumulative))
    display$case_detection_fraction <- ifelse(is.na(display$case_detection_fraction), NA, sprintf("%.3f", display$case_detection_fraction))
    display$simulated_comparable_value <- ifelse(is.na(display$simulated_comparable_value), NA, round(display$simulated_comparable_value))
    display$simulated_to_observed_ratio <- ifelse(is.na(display$simulated_to_observed_ratio), NA, sprintf("%.3f", display$simulated_to_observed_ratio))
    display$relative_error <- ifelse(is.na(display$relative_error), NA, sprintf("%.1f%%", 100 * display$relative_error))
    display$observation_calendar_date <- ifelse(is.na(display$observation_calendar_date) | display$observation_calendar_date == "NA", NA, display$observation_calendar_date)
    display$estimated_event_calendar_date <- ifelse(is.na(display$estimated_event_calendar_date) | display$estimated_event_calendar_date == "NA", NA, display$estimated_event_calendar_date)
    display$estimated_simulated_day0_date <- ifelse(is.na(display$estimated_simulated_day0_date) | display$estimated_simulated_day0_date == "NA", NA, display$estimated_simulated_day0_date)
    names(display) <- c("Target", "Observation day", "Lag days", "Comparable simulation day", "Observed value", "Observed input", "Simulated cumulative", "Detection fraction", "Simulated comparable value", "Sim/Obs ratio", "Relative error", "Observation date", "Estimated event date", "Estimated simulated day-0 date")
    display
  }, rownames = FALSE)

  output$detection_fraction_table <- renderTable({
    if (!isTRUE(input$enable_calibration_targets)) {
      return(data.frame(Message = "Calibration diagnostics are disabled."))
    }
    det <- calculate_detection_fraction_diagnostics()
    if (is.null(det)) return(data.frame(Message = "Run the simulation first."))
    names(det) <- c("Metric", "Value")
    det
  }, rownames = FALSE)


  output$containment_preset_table <- renderTable({
    tbl <- make_containment_preset_table()
    display <- tbl[, c("label", "transmission_reduction_percent", "mobility_reduction_percent", "definition"), drop = FALSE]
    display$transmission_reduction_percent <- ifelse(is.na(display$transmission_reduction_percent), "User-defined", paste0(display$transmission_reduction_percent, "%"))
    display$mobility_reduction_percent <- ifelse(is.na(display$mobility_reduction_percent), "User-defined", paste0(display$mobility_reduction_percent, "%"))
    names(display) <- c("Preset", "Transmission reduction", "Mobility reduction", "Explicit definition")
    display
  }, rownames = FALSE)

  output$containment_schedule_table <- renderTable({
    if (is.null(sim_data$hantavirus) || is.null(sim_data$hantavirus$containment_schedule)) {
      schedule <- make_containment_schedule(
        enabled = ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))),
        preset = input$containment_preset,
        start_day = input$containment_start_day,
        end_day = input$containment_end_day,
        custom_transmission_reduction = input$containment_transmission_reduction,
        custom_mobility_reduction = input$containment_mobility_reduction,
        label = input$containment_label,
        geographic_scope = input$containment_geographic_scope,
        affected_continents = input$containment_affected_continents,
        affected_countries = input$containment_affected_countries
      )
    } else {
      schedule <- sim_data$hantavirus$containment_schedule
    }
    format_containment_schedule_for_display(schedule)
  }, rownames = FALSE)


  output$horizon_diagnostic_table <- renderTable({
    if (is.null(sim_data$hantavirus)) {
      return(data.frame(Metric = "Horizon diagnostic", Value = "Run the simulation first."))
    }
    calculate_horizon_diagnostics(
      sim_result = sim_data$hantavirus,
      mortality_rate = isolate(input$mortality_rate) / 100,
      world_population = WORLD_POPULATION
    )
  }, rownames = FALSE)

  output$containment_report <- renderText({
    if (is.null(sim_data$hantavirus) || is.null(sim_data$hantavirus$containment_schedule)) {
      schedule <- make_containment_schedule(
        enabled = ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))),
        preset = input$containment_preset,
        start_day = input$containment_start_day,
        end_day = input$containment_end_day,
        custom_transmission_reduction = input$containment_transmission_reduction,
        custom_mobility_reduction = input$containment_mobility_reduction,
        label = input$containment_label,
        geographic_scope = input$containment_geographic_scope,
        affected_continents = input$containment_affected_continents,
        affected_countries = input$containment_affected_countries
      )
    } else {
      schedule <- sim_data$hantavirus$containment_schedule
    }
    lines <- c(
      "CONTAINMENT_REPORT_START",
      paste0("containment_enabled=", schedule$enabled),
      paste0("containment_preset=", schedule$preset),
      paste0("containment_preset_label=", schedule$preset_label),
      paste0("containment_definition=", schedule$definition),
      paste0("containment_geographic_scope=", schedule$geographic_scope),
      paste0("containment_affected_continents=", paste(schedule$affected_continents, collapse = ";")),
      paste0("containment_affected_country_count=", schedule$affected_country_count),
      paste0("containment_affected_countries=", paste(schedule$affected_countries, collapse = ";")),
      paste0("containment_mobility_application_rule=", schedule$mobility_application_rule),
      paste0("containment_label=", schedule$label),
      paste0("containment_start_day=", schedule$start_day),
      paste0("containment_end_day=", schedule$end_day),
      paste0("containment_active_days=", schedule$active_days),
      paste0("containment_transmission_reduction_percent=", schedule$transmission_reduction_percent),
      paste0("containment_mobility_reduction_percent=", schedule$mobility_reduction_percent),
      paste0("containment_transmission_multiplier=", sprintf("%.4f", schedule$transmission_multiplier)),
      paste0("containment_mobility_multiplier=", sprintf("%.4f", schedule$mobility_multiplier)),
      paste0("containment_applies_to=", schedule$applies_to),
      paste0("fixed_omicron_rds_modified=", schedule$fixed_reference_modified)
    )
    if (!is.null(sim_data$hantavirus)) {
      hz <- calculate_horizon_diagnostics(sim_data$hantavirus, isolate(input$mortality_rate) / 100, WORLD_POPULATION)
      for (i in seq_len(nrow(hz))) {
        key <- gsub("[^A-Za-z0-9]+", "_", tolower(hz$Metric[i]))
        lines <- c(lines, paste0(key, "=", hz$Value[i]))
      }
    }
    lines <- c(lines,
      "CONTAINMENT_REPORT_END"
    )
    paste(lines, collapse = "\n")
  })

  output$calibration_report <- renderText({
    if (!isTRUE(input$enable_calibration_targets)) return("CALIBRATION_REPORT_DISABLED")
    req(sim_data$hantavirus)
    diag <- calculate_calibration_diagnostics()
    if (is.null(diag)) return("CALIBRATION_REPORT_NOT_AVAILABLE")
    lines <- c("CALIBRATION_REPORT_START")
    for (i in seq_len(nrow(diag))) {
      prefix <- diag$target[i]
      lines <- c(lines,
        paste0(prefix, "_observation_day=", diag$observation_day[i]),
        paste0(prefix, "_lag_days=", diag$lag_days[i]),
        paste0(prefix, "_comparable_simulation_day=", diag$comparable_simulation_day[i]),
        paste0(prefix, "_observed_value=", ifelse(is.na(diag$observed_value[i]), NA, round(diag$observed_value[i]))),
        paste0(prefix, "_simulated_cumulative=", ifelse(is.na(diag$simulated_cumulative[i]), NA, round(diag$simulated_cumulative[i]))),
        paste0(prefix, "_case_detection_fraction=", ifelse(is.na(diag$case_detection_fraction[i]), NA, sprintf("%.6f", diag$case_detection_fraction[i]))),
        paste0(prefix, "_simulated_comparable_value=", ifelse(is.na(diag$simulated_comparable_value[i]), NA, round(diag$simulated_comparable_value[i]))),
        paste0(prefix, "_simulated_to_observed_ratio=", ifelse(is.na(diag$simulated_to_observed_ratio[i]), NA, sprintf("%.6f", diag$simulated_to_observed_ratio[i]))),
        paste0(prefix, "_relative_error=", ifelse(is.na(diag$relative_error[i]), NA, sprintf("%.6f", diag$relative_error[i]))),
        paste0(prefix, "_observation_calendar_date=", diag$observation_calendar_date[i]),
        paste0(prefix, "_estimated_event_calendar_date=", diag$estimated_event_calendar_date[i]),
        paste0(prefix, "_estimated_simulated_day0_date=", diag$estimated_simulated_day0_date[i])
      )
    }
    det <- calculate_detection_fraction_diagnostics()
    if (!is.null(det)) {
      lines <- c(lines, "DETECTION_FRACTION_REPORT_START")
      for (i in seq_len(nrow(det))) {
        lines <- c(lines, paste0(det$metric[i], "=", det$value[i]))
      }
      lines <- c(lines, "DETECTION_FRACTION_REPORT_END")
    }
    lines <- c(lines, "CALIBRATION_REPORT_END")
    paste(lines, collapse = "\n")
  })

  output$evaluation_report <- renderText({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    d <- h$data
    peak_idx <- which.max(d$I)
    cp <- h$cost_profile
    age_mode <- if (!is.null(input$age_parameter_mode)) input$age_parameter_mode else "not_applicable"
    lines <- c(
      "EVALUATION_REPORT_START",
      paste0("app_file=app_epidemiologic_v16_scenario_lab_step6b_basic_exposed_parameter.R"),
      paste0("active_scenario_mode=", isolate(input$active_scenario_mode)),
      paste0("technical_validation_options_visible=", isTRUE(isolate(input$show_technical_validation_options))),
      paste0("model_structure=", h$model_structure),
      paste0("active_compartment_model=", ifelse(is.null(cp$active_compartment_model), ifelse(!is.null(h$compartment_model), h$compartment_model, NA), cp$active_compartment_model)),
      paste0("active_exposed_period_days=", ifelse(is.null(cp$active_exposed_period_days), ifelse(!is.null(h$exposed_period_days), h$exposed_period_days, NA), cp$active_exposed_period_days)),
      paste0("fixed_comparator_compartment_model=", ifelse(is.null(cp$fixed_comparator_compartment_model), NA, cp$fixed_comparator_compartment_model)),
      paste0("comparator_structure_warning=", ifelse(is.null(cp$comparator_structure_warning), NA, cp$comparator_structure_warning)),
      paste0("age_parameter_mode=", age_mode),
      paste0("age_distribution_mode=", ifelse(is.null(cp$age_distribution_mode), isolate(input$age_distribution_mode), cp$age_distribution_mode)),
      paste0("country_age_distribution_source=", ifelse(is.null(cp$country_age_distribution_source), NA, cp$country_age_distribution_source)),
      paste0("country_age_distribution_file=", ifelse(is.null(cp$country_age_distribution_file), NA, cp$country_age_distribution_file)),
      paste0("country_age_distribution_searched_dirs=", ifelse(is.null(cp$country_age_distribution_searched_dirs), NA, cp$country_age_distribution_searched_dirs)),
      paste0("country_polygon_source=", ifelse(is.null(cp$country_polygon_source), NA, cp$country_polygon_source)),
      paste0("country_polygon_file=", ifelse(is.null(cp$country_polygon_file), NA, cp$country_polygon_file)),
      paste0("starting_country=", ifelse(is.null(h$starting_country_used), isolate(input$starting_country), h$starting_country_used)),
      paste0("starting_country_mode=", ifelse(is.null(cp$starting_country_mode), isolate(input$starting_country_mode), cp$starting_country_mode)),
      paste0("starting_country_selection_source=", ifelse(is.null(cp$starting_country_selection_source), "not_recorded", cp$starting_country_selection_source)),
      paste0("guided_random_country_clicks=", ifelse(is.null(cp$guided_random_country_clicks), NA, cp$guided_random_country_clicks)),
      paste0("stochastic_seed=", ifelse(identical(isolate(input$active_scenario_mode), "guided"), "hidden_guided_seed", ifelse(nchar(trimws(isolate(input$stochastic_seed))) == 0, "blank_random", trimws(isolate(input$stochastic_seed))))),
      paste0("effective_seed_used=", ifelse(is.null(cp$effective_seed_used), ifelse(nchar(trimws(isolate(input$stochastic_seed))) == 0, "blank_random", trimws(isolate(input$stochastic_seed))), cp$effective_seed_used)),
      paste0("seed_visibility_mode=", ifelse(is.null(cp$seed_visibility_mode), ifelse(identical(isolate(input$active_scenario_mode), "guided"), "hidden_guided_seed", "advanced_user_seed"), cp$seed_visibility_mode)),
      paste0("scenario_internal_seed_storage=", ifelse(identical(isolate(input$active_scenario_mode), "guided"), "stored_for_future_json_not_shown_in_basic_ui", "user_provided_or_blank")),
      paste0("reproducibility_warning=", ifelse(is.null(cp$reproducibility_warning), "none", cp$reproducibility_warning)),
      paste0("simulation_days=", max(d$time, na.rm = TRUE)),
      paste0("R0_input=", isolate(input$R0)),
      paste0("exposed_period_days=", ifelse(is.null(cp$active_exposed_period_days), NA, cp$active_exposed_period_days)),
      paste0("active_phase_duration_days=", isolate(input$infectious_period_days)),
      paste0("infectious_window_days=", isolate(input$infectious_period_days)),
      paste0("mortality_input_percent=", isolate(input$mortality_rate)),
      paste0("dynamic_module=", ifelse(is.null(cp$dynamic_module), ifelse(isTRUE(get_dynamic_config()$enabled), "active", "off"), cp$dynamic_module)),
      paste0("dynamic_scenario=", ifelse(is.null(cp$dynamic_scenario), get_dynamic_config()$scenario, cp$dynamic_scenario)),
      paste0("dynamic_scenario_label=", ifelse(is.null(cp$dynamic_scenario_label), get_dynamic_config()$scenario_label, cp$dynamic_scenario_label)),
      paste0("dynamic_rate=", ifelse(is.null(cp$dynamic_rate), get_dynamic_config()$rate, cp$dynamic_rate)),
      paste0("dynamic_opportunity_multiplier=", ifelse(is.null(cp$dynamic_effective_targets), get_dynamic_config()$targets, cp$dynamic_effective_targets)),
      paste0("dynamic_calibration=", ifelse(is.null(cp$dynamic_calibration), get_dynamic_config()$calibration, cp$dynamic_calibration)),
      paste0("dynamic_max_R0_multiplier=", ifelse(is.null(cp$dynamic_max_R0_multiplier), get_dynamic_config()$max_multiplier, cp$dynamic_max_R0_multiplier)),
      paste0("dynamic_adaptive_saturation=", ifelse(isTRUE(get_dynamic_config()$adaptive_saturation), "TRUE", "FALSE")),
      paste0("dynamic_saturation_exponent=", get_dynamic_config()$saturation_exponent),
      paste0("dynamic_preset_interpretation=", ifelse(is.null(get_dynamic_config()$interpretation), "not_available", get_dynamic_config()$interpretation)),
      paste0("dynamic_candidate_events=", ifelse(is.null(cp$dynamic_candidate_events), NA, cp$dynamic_candidate_events)),
      paste0("dynamic_established_events=", ifelse(is.null(cp$dynamic_established_events), NA, cp$dynamic_established_events)),
      paste0("dynamic_initial_R0=", ifelse(is.null(cp$dynamic_initial_R0), NA, cp$dynamic_initial_R0)),
      paste0("dynamic_final_effective_R0=", ifelse(is.null(cp$dynamic_final_effective_R0), NA, cp$dynamic_final_effective_R0)),
      paste0("dynamic_max_R0_multiplier_requested=", ifelse(is.null(cp$dynamic_max_R0_multiplier_requested), NA, cp$dynamic_max_R0_multiplier_requested)),
      paste0("dynamic_R0_cap_requested=", ifelse(is.null(cp$dynamic_R0_cap_requested), NA, cp$dynamic_R0_cap_requested)),
      paste0("dynamic_max_candidate_R0_observed=", ifelse(is.null(cp$dynamic_max_candidate_R0_observed), NA, cp$dynamic_max_candidate_R0_observed)),
      paste0("dynamic_max_candidate_advantage_observed=", ifelse(is.null(cp$dynamic_max_candidate_advantage_observed), NA, cp$dynamic_max_candidate_advantage_observed)),
      paste0("dynamic_max_established_variant_R0=", ifelse(is.null(cp$dynamic_max_established_variant_R0), NA, cp$dynamic_max_established_variant_R0)),
      paste0("dynamic_max_established_variant_advantage=", ifelse(is.null(cp$dynamic_max_established_variant_advantage), NA, cp$dynamic_max_established_variant_advantage)),
      paste0("dynamic_mean_established_variant_R0=", ifelse(is.null(cp$dynamic_mean_established_variant_R0), NA, cp$dynamic_mean_established_variant_R0)),
      paste0("dynamic_dominant_variant_id_final=", ifelse(is.null(cp$dynamic_dominant_variant_id_final), NA, cp$dynamic_dominant_variant_id_final)),
      paste0("dynamic_dominant_variant_share_final=", ifelse(is.null(cp$dynamic_dominant_variant_share_final), NA, cp$dynamic_dominant_variant_share_final)),
      paste0("dynamic_dominant_variant_R0_final=", ifelse(is.null(cp$dynamic_dominant_variant_R0_final), NA, cp$dynamic_dominant_variant_R0_final)),
      paste0("dynamic_candidate_cap_binding_events=", ifelse(is.null(cp$dynamic_candidate_cap_binding_events), NA, cp$dynamic_candidate_cap_binding_events)),
      paste0("dynamic_established_cap_binding_events=", ifelse(is.null(cp$dynamic_established_cap_binding_events), NA, cp$dynamic_established_cap_binding_events)),
      paste0("dynamic_cap_usage_note=", ifelse(is.null(cp$dynamic_cap_usage_note), NA, cp$dynamic_cap_usage_note)),
      paste0("dynamic_monotonicity_note=", ifelse(is.null(cp$dynamic_monotonicity_note), NA, cp$dynamic_monotonicity_note)),
      paste0("dynamic_adaptive_saturation=", ifelse(is.null(cp$dynamic_adaptive_saturation), ifelse(isTRUE(get_dynamic_config()$adaptive_saturation), "TRUE", "FALSE"), cp$dynamic_adaptive_saturation)),
      paste0("dynamic_saturation_exponent=", ifelse(is.null(cp$dynamic_saturation_exponent), get_dynamic_config()$saturation_exponent, cp$dynamic_saturation_exponent)),
      paste0("dynamic_mean_adaptive_space_multiplier=", ifelse(is.null(cp$dynamic_mean_adaptive_space_multiplier), NA, cp$dynamic_mean_adaptive_space_multiplier)),
      paste0("dynamic_max_expressed_advantage_observed=", ifelse(is.null(cp$dynamic_max_expressed_advantage_observed), NA, cp$dynamic_max_expressed_advantage_observed)),
      paste0("dynamic_runtime_overhead_seconds=", ifelse(is.null(cp$dynamic_runtime_overhead_seconds), NA, cp$dynamic_runtime_overhead_seconds)),
      paste0("target_weighted_cfr=", ifelse(is.null(cp$target_weighted_cfr), NA, cp$target_weighted_cfr)),
      paste0("achieved_weighted_cfr=", ifelse(is.null(cp$achieved_weighted_cfr), NA, cp$achieved_weighted_cfr)),
      paste0("cfr_saturated_groups=", ifelse(is.null(cp$cfr_saturated_groups), NA, cp$cfr_saturated_groups)),
      paste0("min_age_cfr=", ifelse(is.null(cp$min_age_cfr), NA, cp$min_age_cfr)),
      paste0("max_age_cfr=", ifelse(is.null(cp$max_age_cfr), NA, cp$max_age_cfr)),
      paste0("cfr_warning=", ifelse(is.null(cp$cfr_warning), NA, cp$cfr_warning)),
      paste0("calibration_enabled=", isTRUE(isolate(input$enable_calibration_targets))),
      paste0("calibration_population=", isolate(input$calibration_population)),
      paste0("observed_positive_cumulative=", isolate(input$observed_positive_cumulative)),
      paste0("case_detection_fraction=", isolate(input$case_detection_fraction)),
      paste0("estimate_detection_from_serology=", isTRUE(isolate(input$estimate_detection_from_serology))),
      paste0("day0_date_estimation_enabled=", isTRUE(isolate(input$enable_day0_date_estimation))),
      paste0("observed_positive_calendar_date=", as.character(isolate(input$observed_positive_calendar_date))),
      paste0("observed_seroprevalence_calendar_date=", as.character(isolate(input$observed_seroprevalence_calendar_date))),
      paste0("observed_positive_day=", isolate(input$observed_positive_day)),
      paste0("positive_lag_days=", isolate(input$positive_lag_days)),
      paste0("observed_seroprevalence_percent=", isolate(input$observed_seroprevalence_percent)),
      paste0("observed_seroprevalence_day=", isolate(input$observed_seroprevalence_day)),
      paste0("seroprevalence_lag_days=", isolate(input$seroprevalence_lag_days)),
      paste0("containment_enabled=", ifelse(is.null(cp$containment_enabled), ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))), cp$containment_enabled)),
      paste0("containment_preset=", ifelse(is.null(cp$containment_preset), isolate(input$containment_preset), cp$containment_preset)),
      paste0("containment_start_day=", ifelse(is.null(cp$containment_start_day), isolate(input$containment_start_day), cp$containment_start_day)),
      paste0("containment_end_day=", ifelse(is.null(cp$containment_end_day), isolate(input$containment_end_day), cp$containment_end_day)),
      paste0("containment_transmission_reduction_percent=", ifelse(is.null(cp$containment_transmission_reduction_percent), NA, cp$containment_transmission_reduction_percent)),
      paste0("containment_mobility_reduction_percent=", ifelse(is.null(cp$containment_mobility_reduction_percent), NA, cp$containment_mobility_reduction_percent)),
      paste0("containment_transmission_multiplier=", ifelse(is.null(cp$containment_transmission_multiplier), NA, cp$containment_transmission_multiplier)),
      paste0("containment_mobility_multiplier=", ifelse(is.null(cp$containment_mobility_multiplier), NA, cp$containment_mobility_multiplier)),
      paste0("containment_geographic_scope=", ifelse(is.null(cp$containment_geographic_scope), isolate(input$containment_geographic_scope), cp$containment_geographic_scope)),
      paste0("containment_affected_continents=", ifelse(is.null(cp$containment_affected_continents), paste(isolate(input$containment_affected_continents), collapse = ";"), cp$containment_affected_continents)),
      paste0("containment_affected_country_count=", ifelse(is.null(cp$containment_affected_country_count), NA, cp$containment_affected_country_count)),
      paste0("containment_affected_countries=", ifelse(is.null(cp$containment_affected_countries), paste(isolate(input$containment_affected_countries), collapse = ";"), cp$containment_affected_countries)),
      paste0("containment_mobility_application_rule=", ifelse(is.null(cp$containment_mobility_application_rule), NA, cp$containment_mobility_application_rule)),
      paste0("air_travel_scenario=", isolate(input$air_travel_scenario)),
      paste0("import_establishment_probability=", isolate(input$import_establishment_probability)),
      paste0("final_exposed=", ifelse("E" %in% names(d), round(tail(d$E, 1)), ifelse(is.null(cp$final_exposed), NA, cp$final_exposed))),
      paste0("final_active=", round(tail(d$I, 1))),
      paste0("final_recovered=", round(tail(d$R, 1))),
      paste0("final_deaths=", round(tail(d$D, 1))),
      paste0("final_active_percent_population=", ifelse(is.null(cp$final_active_percent_population), NA, cp$final_active_percent_population)),
      paste0("final_active_warning=", ifelse(is.null(cp$final_active_warning), NA, cp$final_active_warning)),
      paste0("projected_additional_deaths_if_active_resolved=", ifelse(is.null(cp$projected_additional_deaths_if_active_resolved), NA, cp$projected_additional_deaths_if_active_resolved)),
      paste0("projected_final_deaths_if_active_resolved=", ifelse(is.null(cp$projected_final_deaths_if_active_resolved), NA, cp$projected_final_deaths_if_active_resolved)),
      paste0("peak_exposed=", ifelse(is.null(cp$peak_exposed), ifelse("E" %in% names(d), round(max(d$E, na.rm = TRUE)), NA), cp$peak_exposed)),
      paste0("peak_exposed_day=", ifelse(is.null(cp$peak_exposed_day), ifelse("E" %in% names(d), d$time[which.max(d$E)], NA), cp$peak_exposed_day)),
      paste0("peak_active=", round(d$I[peak_idx])),
      paste0("peak_day=", d$time[peak_idx]),
      paste0("countries_reached=", sum(!is.na(h$first_reached_day))),
      paste0("expected_imported_seeds_total=", round(sum(h$expected_import_history, na.rm = TRUE), 4)),
      paste0("run_event_elapsed_seconds=", ifelse(is.null(cp$run_event_elapsed_seconds), NA, cp$run_event_elapsed_seconds)),
      paste0("engine_elapsed_seconds=", ifelse(is.null(cp$elapsed_seconds), NA, cp$elapsed_seconds)),
      paste0("active_cells=", ifelse(is.null(cp$active_cells), NA, cp$active_cells)),
      paste0("age_groups=", ifelse(is.null(cp$age_groups), NA, cp$age_groups)),
      paste0("rds_reference=", ifelse(is.null(cp$rds_reference), NA, cp$rds_reference)),
      "EVALUATION_REPORT_END"
    )

    if (!is.null(h$age_summary)) {
      a <- h$age_summary
      age_lines <- paste0(
        "age_group=", a$age_group,
        ";population=", round(a$population),
        ";exposed_final=", ifelse("exposed_final" %in% names(a), round(a$exposed_final), NA),
        ";active_final=", round(a$active_final),
        ";resolved=", round(a$cumulative_resolved),
        ";deaths=", round(a$deaths),
        ";cfr_applied=", signif(a$cfr, 6),
        ";death_share=", signif(a$death_share, 6)
      )
      lines <- c(lines[1:(length(lines)-1)], "AGE_SUMMARY_START", age_lines, "AGE_SUMMARY_END", lines[length(lines)])
    }

    paste(lines, collapse = "\n")
  })

  output$assumptions_table <- renderTable({
    data.frame(
      Component = c(
        "Country dynamics", "Infection-age structure", "Passenger traffic", "International spread", "Imported establishment", "Representation diagnostics", "Baseline state", "Variant emergence", "Variant replacement", "COVID-19 Omicron comparator"
      ),
      Assumption = c(
        "Each country has separate S, E, I, R and D compartments when SEIRD is active; SIRD remains available for sensitivity analysis.",
        "Infections are stored by days since infection. A normalized Gaussian-like or flat infectivity kernel determines current infectious pressure.",
        "Diffusion uses an explicit bilateral annual passenger-flow matrix plus a small residual air-traffic component for unlisted routes.",
        "Expected infectious travellers are computed from passenger flow and infection-age-specific prevalence in the origin country.",
        "A traveller's probability of seeding local transmission depends on destination Rt and remaining infectiousness, with a user-visible opportunity modifier.",
        "Diagnostic tables compare internal imports, country_history, display thresholds and map eligibility to identify representation failures.",
        "At day 0 only the selected starting country is infected. Other countries can only receive imported infections after simulation time advances.",
        "Candidate macro-variants depend on incident infections, mutation rate, effective transmission-relevant sites and a calibration level.",
        "Only beneficial established variants enter the circulating pool; their frequencies evolve by relative effective R0, capped to avoid unrealistic runaway values.",
        "The COVID-19 Omicron reference starts in South Africa, uses Omicron-like reference parameters, disables further variant evolution and is loaded from an RDS file rather than recomputed after each user simulation."
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  run_simulation <- function(R0_target,
                             infectious_period_days,
                             mortality_rate,
                             N,
                             I0,
                             days,
                             mutation_rate_per_replication,
                             effective_mutation_targets,
                             variant_emergence_calibration,
                             maximum_variant_R0_multiplier,
                             starting_country,
                             air_travel_scenario,
                             import_establishment_probability,
                             infectiousness_profile = "mid",
                             enable_evolution,
                             rng_seed,
                             progress_callback = NULL,
                             containment_schedule = NULL,
                             dynamic_adaptive_saturation = TRUE,
                             dynamic_saturation_exponent = 1.0) {

    if (!is.null(rng_seed) && is.finite(rng_seed)) set.seed(as.integer(rng_seed))

    countries <- COUNTRIES_LIST
    t <- seq(0, days, by = 1)
    result <- data.frame(time = t)

    params <- COUNTRY_TRAVEL_PARAMS[match(countries, COUNTRY_TRAVEL_PARAMS$country), ]
    country_populations <- setNames(params$population_millions / sum(params$population_millions) * N, countries)

    passenger_matrix_annual <- make_bilateral_passenger_matrix(
      countries = countries,
      air_travel_scenario = air_travel_scenario,
      rng_seed = rng_seed,
      route_noise_sdlog = 0.25,
      residual_fraction = 0.05
    )
    passenger_matrix_daily <- passenger_matrix_annual / 365

    # Infection-age kernel. Duration is fixed in discrete time. The kernel sums
    # to 1, so R0 remains interpretable as total secondary infections per case.
    infectious_duration <- max(1, as.integer(round(infectious_period_days)))
    infectivity_kernel <- make_infectivity_kernel(infectious_duration, infectiousness_profile)
    remaining_infectivity <- rev(cumsum(rev(infectivity_kernel)))

    S_country <- country_populations
    R_country <- setNames(rep(0, length(countries)), countries)
    D_country <- setNames(rep(0, length(countries)), countries)

    # Rows = countries, columns = infection-age 0...(D-1).
    I_age <- matrix(
      0,
      nrow = length(countries),
      ncol = infectious_duration,
      dimnames = list(countries, paste0("age_", 0:(infectious_duration - 1)))
    )
    I_age[starting_country, 1] <- I0
    S_country[starting_country] <- max(0, S_country[starting_country] - I0)
    I_country <- setNames(rowSums(I_age), countries)

    country_history <- matrix(0, nrow = length(t), ncol = length(countries), dimnames = list(NULL, countries))
    death_country_history <- matrix(0, nrow = length(t), ncol = length(countries), dimnames = list(NULL, countries))
    import_history <- matrix(0, nrow = length(t), ncol = length(countries), dimnames = list(NULL, countries))
    expected_import_history <- matrix(0, nrow = length(t), ncol = length(countries), dimnames = list(NULL, countries))
    infectious_pressure_history <- matrix(0, nrow = length(t), ncol = length(countries), dimnames = list(NULL, countries))
    first_reached_day <- setNames(rep(NA_real_, length(countries)), countries)
    first_reached_day[starting_country] <- 0

    base_R0 <- R0_target
    base_beta <- R0_target / infectious_duration
    R0_cap <- R0_target * maximum_variant_R0_multiplier

    variants_emerged <- data.frame(
      day = 0,
      variant_id = 1,
      parent_variant_id = NA_integer_,
      origin_country = starting_country,
      beta_value = base_beta,
      R0_value = base_R0,
      fitness_advantage = 0,
      establishment_probability = 1,
      mutation_class = "Founder",
      stringsAsFactors = FALSE
    )
    candidate_mutations <- data.frame(
      day = integer(), origin_country = character(), mutation_class = character(), raw_fitness_effect = numeric(), expressed_fitness_effect = numeric(), adaptive_space_multiplier = numeric(),
      establishment_probability = numeric(), established = logical(), candidate_R0 = numeric(), stringsAsFactors = FALSE
    )
    variant_count <- 1
    variant_freq <- setNames(1, "1")
    variant_frequency_history <- data.frame(day = integer(), variant_id = integer(), frequency = numeric(), R0_value = numeric())
    R0_history <- data.frame(day = integer(), effective_R0 = numeric(), dominant_variant = integer(), number_variants = integer())

    calib <- variant_calibration_values(variant_emergence_calibration)

    draw_mutation_effect <- function() {
      mutation_class <- sample(
        c("Deleterious", "Neutral", "Small beneficial", "Moderate beneficial", "Large beneficial"),
        size = 1,
        prob = c(0.70, 0.24, 0.05, 0.009, 0.001)
      )
      effect <- switch(
        mutation_class,
        "Deleterious" = -runif(1, min = 0.005, max = 0.15),
        "Neutral" = rnorm(1, mean = 0, sd = 0.0015),
        "Small beneficial" = runif(1, min = 0.005, max = 0.035),
        "Moderate beneficial" = runif(1, min = 0.035, max = 0.10),
        "Large beneficial" = runif(1, min = 0.10, max = 0.20)
      )
      list(class = mutation_class, effect = effect)
    }

    for (i in seq_along(t)) {
      day <- t[i]
      if (!is.null(progress_callback) && (day == 0 || day %% 10 == 0 || day == days)) {
        progress_callback(day, days)
      }
      imported_cases_today <- setNames(rep(0, length(countries)), countries)
      expected_imports_today <- setNames(rep(0, length(countries)), countries)
      daily_new_infections_total <- 0
      containment_today <- containment_multipliers_for_day(containment_schedule, day)
      transmission_multiplier_by_country <- containment_transmission_vector_for_day(containment_schedule, day, countries)

      # Current country-level active infections and infectious pressure.
      I_country <- setNames(rowSums(I_age), countries)
      infectious_pressure <- keep_country_names(as.numeric(I_age %*% infectivity_kernel), countries)

      if (day > 0) {
        # Variant replacement dynamic.
        variant_ids_chr <- as.character(variants_emerged$variant_id)
        variant_freq <- variant_freq[variant_ids_chr]
        variant_freq[is.na(variant_freq)] <- 0
        names(variant_freq) <- variant_ids_chr
        if (sum(variant_freq) <= 0) variant_freq[1] <- 1
        variant_freq <- variant_freq / sum(variant_freq)

        if (length(variant_freq) > 1) {
          R0s <- variants_emerged$R0_value
          R0_mean <- sum(variant_freq * R0s)
          if (is.finite(R0_mean) && R0_mean > 0) {
            relative_growth <- log(pmax(R0s, 1e-12) / R0_mean)
            relative_growth <- pmax(-1.2, pmin(1.2, relative_growth))
            variant_freq <- variant_freq * exp(0.85 * relative_growth)
            variant_freq <- variant_freq / sum(variant_freq)
          }
        }
        current_R0_base <- sum(variant_freq * variants_emerged$R0_value)
        current_R0_country <- current_R0_base * transmission_multiplier_by_country
        current_R0 <- weighted.mean(current_R0_country, country_populations)

        # Country-level SIR-D with infection-age structure.
        local_new_infections <- current_R0_country * S_country * infectious_pressure / country_populations
        local_new_infections <- keep_country_names(local_new_infections, countries)
        local_new_infections[!is.finite(local_new_infections)] <- 0
        local_new_infections <- pmin(local_new_infections, S_country)
        local_new_infections <- keep_country_names(local_new_infections, countries)

        # International passenger-traffic spread with infection-age and Rt.
        # This preserves the previous passenger/prevalence logic but replaces the
        # fixed establishment probability with an Rt- and infection-age-dependent
        # probability. It also avoids using names(I_country) after pmax/pmin
        # operations; countries[...] is used explicitly.
        active_origins <- countries[rowSums(I_age) > 0]
        if (length(active_origins) > 0 && import_establishment_probability > 0) {
          available_after_local <- pmax(0, S_country - local_new_infections)
          available_after_local <- keep_country_names(available_after_local, countries)
          Rt_destination <- current_R0_country * available_after_local / country_populations
          Rt_destination <- keep_country_names(Rt_destination, countries)
          Rt_destination[!is.finite(Rt_destination)] <- 0

          # Vectorised equivalent of the previous origin-destination-age loops.
          # Rows are origins, columns are destinations. It preserves the same
          # expected-value assumptions: passenger flow * origin infection-age
          # prevalence * destination Rt/infectivity-dependent establishment.
          origin_age_prevalence <- sweep(I_age, 1, country_populations, "/")
          origin_age_prevalence[!is.finite(origin_age_prevalence)] <- 0
          origin_age_prevalence[!(countries %in% active_origins), ] <- 0

          p_seed_by_destination_age <- t(vapply(
            Rt_destination,
            import_seed_probability,
            remaining_infectivity = remaining_infectivity,
            opportunity_modifier = import_establishment_probability,
            FUN.VALUE = numeric(length(remaining_infectivity))
          ))
          p_seed_by_destination_age[available_after_local <= 0, ] <- 0
          p_seed_by_destination_age[!is.finite(p_seed_by_destination_age)] <- 0

          route_seed_factor <- origin_age_prevalence %*% t(p_seed_by_destination_age)
          passenger_matrix_for_imports <- apply_containment_to_passenger_matrix(passenger_matrix_daily, containment_schedule, day, countries)
          diag(passenger_matrix_for_imports) <- 0
          passenger_matrix_for_imports[!is.finite(passenger_matrix_for_imports)] <- 0

          expected_imports_today <- colSums(passenger_matrix_for_imports * route_seed_factor, na.rm = TRUE)
          expected_imports_today <- keep_country_names(expected_imports_today, countries)
          expected_imports_today[!is.finite(expected_imports_today)] <- 0
          expected_imports_today <- pmin(expected_imports_today, available_after_local)
          expected_imports_today <- keep_country_names(expected_imports_today, countries)

          # This is an expected-value macroscopic simulator. Expected imported
          # seed infections are added directly. Very small numerical noise is
          # retained in expected_import_history but not used as active cases.
          imported_cases_today <- expected_imports_today
          imported_cases_today[imported_cases_today < 1e-8] <- 0
          imported_cases_today <- keep_country_names(imported_cases_today, countries)
        }

        new_cases_today <- local_new_infections + imported_cases_today
        new_cases_today <- pmin(new_cases_today, S_country)
        new_cases_today <- keep_country_names(new_cases_today, countries)
        daily_new_infections_total <- sum(new_cases_today, na.rm = TRUE)

        # Resolve cases that have completed the infectious window, then age the
        # remaining infections and insert new infections at age 0.
        leaving_infections <- keep_country_names(I_age[, infectious_duration], countries)
        deaths <- mortality_rate * leaving_infections
        recoveries <- (1 - mortality_rate) * leaving_infections
        deaths <- keep_country_names(deaths, countries)
        recoveries <- keep_country_names(recoveries, countries)

        S_country <- pmax(0, S_country - new_cases_today)
        S_country <- keep_country_names(S_country, countries)
        R_country <- R_country + recoveries
        R_country <- keep_country_names(R_country, countries)
        D_country <- D_country + deaths
        D_country <- keep_country_names(D_country, countries)

        if (infectious_duration == 1) {
          I_age[, 1] <- new_cases_today
        } else {
          I_age[, 2:infectious_duration] <- I_age[, 1:(infectious_duration - 1), drop = FALSE]
          I_age[, 1] <- new_cases_today
        }
        rownames(I_age) <- countries
        I_country <- keep_country_names(rowSums(I_age), countries)

        newly_reached <- countries[imported_cases_today > 0 & is.na(first_reached_day)]
        if (length(newly_reached) > 0) first_reached_day[newly_reached] <- day

        # Macroscopic stochastic variant emergence.
        I_total <- sum(I_country)
        if (enable_evolution && I_total > 0 && daily_new_infections_total > 0) {
          raw_mutational_opportunities <- daily_new_infections_total * mutation_rate_per_replication * effective_mutation_targets
          candidate_lambda <- raw_mutational_opportunities * calib$observable_fraction
          candidate_lambda <- max(0, min(candidate_lambda, 25))
          num_candidates <- rpois(1, lambda = candidate_lambda)

          if (num_candidates > 0) {
            origin_prob <- pmax(new_cases_today, 0)
            origin_prob[!is.finite(origin_prob)] <- 0
            if (sum(origin_prob) <= 0) origin_prob <- pmax(I_country, 0)
            origin_prob[!is.finite(origin_prob)] <- 0
            if (sum(origin_prob) <= 0) {
              origin_prob <- setNames(rep(1 / length(countries), length(countries)), countries)
            } else {
              origin_prob <- origin_prob / sum(origin_prob)
            }

            for (cand in seq_len(num_candidates)) {
              mut <- draw_mutation_effect()
              raw_effect <- mut$effect
              origin_country <- sample(countries, size = 1, prob = origin_prob)

              dominant_id <- as.integer(names(which.max(variant_freq)))
              parent_R0 <- variants_emerged$R0_value[variants_emerged$variant_id == dominant_id]
              if (length(parent_R0) == 0 || !is.finite(parent_R0)) parent_R0 <- base_R0

              adaptive_space_multiplier <- 1
              if (isTRUE(dynamic_adaptive_saturation) && is.finite(raw_effect) && raw_effect > 0 && is.finite(R0_cap) && is.finite(base_R0) && R0_cap > base_R0) {
                current_reference_R0 <- if (exists("current_R0_base") && is.finite(current_R0_base)) current_R0_base else parent_R0
                current_reference_R0 <- max(base_R0, min(R0_cap, current_reference_R0))
                adaptive_space_multiplier <- (R0_cap - current_reference_R0) / (R0_cap - base_R0)
                adaptive_space_multiplier <- max(0, min(1, adaptive_space_multiplier))
                adaptive_space_multiplier <- adaptive_space_multiplier ^ max(0.01, dynamic_saturation_exponent)
              }
              expressed_effect <- ifelse(is.finite(raw_effect) && raw_effect > 0, raw_effect * adaptive_space_multiplier, raw_effect)

              candidate_R0 <- max(0.01 * base_R0, min(R0_cap, parent_R0 * (1 + expressed_effect)))
              candidate_beta <- candidate_R0 / infectious_duration
              realized_advantage <- (candidate_R0 / parent_R0) - 1

              establishment_probability <- 0
              established <- FALSE
              if (realized_advantage > 0.003) {
                prevalence_factor <- (I_total / (I_total + 1e6)) ^ 0.35
                establishment_probability <- calib$establishment_multiplier * 2.2 * realized_advantage * prevalence_factor
                establishment_probability <- max(0, min(0.35, establishment_probability))
                if (runif(1) < establishment_probability) {
                  established <- TRUE
                  variant_count <- variant_count + 1
                  intro_frequency <- min(0.03, max(0.001, 50 / max(I_total, 1)))
                  variants_emerged <- rbind(
                    variants_emerged,
                    data.frame(
                      day = day,
                      variant_id = variant_count,
                      parent_variant_id = dominant_id,
                      origin_country = origin_country,
                      beta_value = candidate_beta,
                      R0_value = candidate_R0,
                      fitness_advantage = realized_advantage * 100,
                      establishment_probability = establishment_probability,
                      mutation_class = mut$class,
                      stringsAsFactors = FALSE
                    )
                  )
                  variant_freq <- variant_freq * (1 - intro_frequency)
                  variant_freq[as.character(variant_count)] <- intro_frequency
                  variant_freq <- variant_freq / sum(variant_freq)
                }
              }

              candidate_mutations <- rbind(
                candidate_mutations,
                data.frame(
                  day = day,
                  origin_country = origin_country,
                  mutation_class = mut$class,
                  raw_fitness_effect = raw_effect * 100,
                  expressed_fitness_effect = expressed_effect * 100,
                  adaptive_space_multiplier = adaptive_space_multiplier,
                  establishment_probability = establishment_probability,
                  established = established,
                  candidate_R0 = candidate_R0,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
        }
      }

      I_country <- keep_country_names(rowSums(I_age), countries)
      infectious_pressure <- keep_country_names(as.numeric(I_age %*% infectivity_kernel), countries)

      result$S[i] <- sum(S_country)
      result$I[i] <- sum(I_country)
      result$R[i] <- sum(R_country)
      result$D[i] <- sum(D_country)
      country_history[i, ] <- I_country
      death_country_history[i, ] <- D_country
      import_history[i, ] <- imported_cases_today
      expected_import_history[i, ] <- expected_imports_today
      infectious_pressure_history[i, ] <- infectious_pressure

      variant_ids_chr <- as.character(variants_emerged$variant_id)
      variant_freq <- variant_freq[variant_ids_chr]
      variant_freq[is.na(variant_freq)] <- 0
      names(variant_freq) <- variant_ids_chr
      if (sum(variant_freq) <= 0) variant_freq[1] <- 1
      variant_freq <- variant_freq / sum(variant_freq)

      for (vid in variants_emerged$variant_id) {
        vid_chr <- as.character(vid)
        this_freq <- ifelse(vid_chr %in% names(variant_freq), variant_freq[vid_chr], 0)
        this_R0 <- variants_emerged$R0_value[variants_emerged$variant_id == vid][1]
        variant_frequency_history <- rbind(
          variant_frequency_history,
          data.frame(day = day, variant_id = vid, frequency = as.numeric(this_freq), R0_value = this_R0)
        )
      }

      current_R0 <- sum(variant_freq * variants_emerged$R0_value) * containment_today$transmission
      R0_history <- rbind(
        R0_history,
        data.frame(
          day = day,
          effective_R0 = current_R0,
          dominant_variant = as.integer(names(which.max(variant_freq))),
          number_variants = nrow(variants_emerged)
        )
      )
    }

    list(
      data = result,
      variants_emerged = variants_emerged,
      candidate_mutations = candidate_mutations,
      variant_frequency_history = variant_frequency_history,
      R0_history = R0_history,
      country_history = country_history,
      exposed_country_history = exposed_country_history,
      death_country_history = death_country_history,
      new_country_import_history = import_history,
      expected_import_history = expected_import_history,
      infectious_pressure_history = infectious_pressure_history,
      first_reached_day = first_reached_day,
      passenger_matrix_annual = passenger_matrix_annual,
      infectivity_kernel = infectivity_kernel,
      remaining_infectivity = remaining_infectivity,
      countries = countries,
      final_R0 = round(tail(R0_history$effective_R0, 1), 2),
      total_variants = max(0, nrow(variants_emerged) - 1)
    )
  }


  run_simulation_age_adjusted <- function(R0_target,
                                          infectious_period_days,
                                          mortality_rate,
                                          N,
                                          I0,
                                          days,
                                          mutation_rate_per_replication,
                                          effective_mutation_targets,
                                          variant_emergence_calibration,
                                          maximum_variant_R0_multiplier,
                                          enable_evolution,
                                          starting_country,
                                          air_travel_scenario,
                                          import_establishment_probability,
                                          infectiousness_profile = "mid",
                                          rng_seed,
                                          progress_callback = NULL,
                                          neutral_age_weights = TRUE,
                                          age_cfr_scale = 1,
                                          containment_schedule = NULL,
                                          dynamic_adaptive_saturation = TRUE,
                                          dynamic_saturation_exponent = 1.0,
                                          age_distribution_mode = "country_specific",
                                          disease_compartment_model = "SEIRD",
                                          exposed_period_days = 4) {

    start_time <- Sys.time()
    if (!is.null(rng_seed) && is.finite(rng_seed)) set.seed(as.integer(rng_seed))

    countries <- COUNTRIES_LIST
    t <- seq(0, days, by = 1)
    result <- data.frame(time = t)

    params <- COUNTRY_TRAVEL_PARAMS[match(countries, COUNTRY_TRAVEL_PARAMS$country), ]
    country_populations <- setNames(params$population_millions / sum(params$population_millions) * N, countries)

    passenger_matrix_annual <- make_bilateral_passenger_matrix(
      countries = countries,
      air_travel_scenario = air_travel_scenario,
      rng_seed = rng_seed,
      route_noise_sdlog = 0.25,
      residual_fraction = 0.05
    )
    passenger_matrix_daily <- passenger_matrix_annual / 365

    infectious_duration <- max(1, as.integer(round(infectious_period_days)))
    infectivity_kernel <- make_infectivity_kernel(infectious_duration, infectiousness_profile)
    remaining_infectivity <- rev(cumsum(rev(infectivity_kernel)))

    disease_compartment_model <- toupper(as.character(disease_compartment_model))
    if (!disease_compartment_model %in% c("SIRD", "SEIRD")) disease_compartment_model <- "SEIRD"
    use_seird <- identical(disease_compartment_model, "SEIRD")
    exposed_duration <- max(1, as.integer(round(exposed_period_days)))
    if (!isTRUE(use_seird)) exposed_duration <- 0L

    age_params <- make_age_group_parameters(mortality_rate = mortality_rate, neutral = neutral_age_weights, cfr_scale = age_cfr_scale)
    age_groups <- age_params$age_group
    n_countries <- length(countries)
    n_age <- length(age_groups)

    country_age_distribution <- make_country_age_distribution(countries, mode = age_distribution_mode)
    age_distribution_source <- attr(country_age_distribution, "source")
    if (is.null(age_distribution_source) || length(age_distribution_source) == 0) age_distribution_source <- NA_character_
    age_distribution_file <- attr(country_age_distribution, "file")
    if (is.null(age_distribution_file) || length(age_distribution_file) == 0) age_distribution_file <- NA_character_
    model_age_share <- model_age_share_from_country_distribution(country_populations, country_age_distribution)
    age_params <- update_age_params_for_model_age_share(
      age_params = age_params,
      model_age_share = model_age_share,
      mortality_rate = mortality_rate,
      neutral = neutral_age_weights,
      cfr_scale = age_cfr_scale
    )

    S_age <- sweep(country_age_distribution, 1, country_populations, "*")
    dimnames(S_age) <- list(countries, age_groups)
    R_age <- S_age * 0
    D_age <- S_age * 0

    I_arr <- array(
      0,
      dim = c(n_countries, n_age, infectious_duration),
      dimnames = list(countries, age_groups, paste0("age_", 0:(infectious_duration - 1)))
    )
    E_arr <- if (isTRUE(use_seird)) {
      array(
        0,
        dim = c(n_countries, n_age, exposed_duration),
        dimnames = list(countries, age_groups, paste0("exposed_age_", 0:(exposed_duration - 1)))
      )
    } else {
      NULL
    }
    initial_by_age <- I0 * age_params$initial_distribution_weight
    I_arr[starting_country, , 1] <- initial_by_age
    S_age[starting_country, ] <- pmax(0, S_age[starting_country, ] - initial_by_age)

    country_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    exposed_country_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    death_country_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    import_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    expected_import_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    infectious_pressure_history <- matrix(0, nrow = length(t), ncol = n_countries, dimnames = list(NULL, countries))
    first_reached_day <- setNames(rep(NA_real_, n_countries), countries)
    first_reached_day[starting_country] <- 0

    age_history <- data.frame()
    base_R0 <- R0_target
    base_beta <- R0_target / infectious_duration
    R0_cap <- R0_target * maximum_variant_R0_multiplier

    variants_emerged <- data.frame(
      day = 0,
      variant_id = 1,
      parent_variant_id = NA_integer_,
      origin_country = starting_country,
      beta_value = base_beta,
      R0_value = base_R0,
      fitness_advantage = 0,
      establishment_probability = 1,
      mutation_class = "Founder",
      stringsAsFactors = FALSE
    )
    candidate_mutations <- data.frame(
      day = integer(), origin_country = character(), mutation_class = character(), raw_fitness_effect = numeric(), expressed_fitness_effect = numeric(), adaptive_space_multiplier = numeric(),
      establishment_probability = numeric(), established = logical(), candidate_R0 = numeric(), stringsAsFactors = FALSE
    )
    variant_count <- 1
    variant_freq <- setNames(1, "1")
    variant_frequency_history <- data.frame(day = integer(), variant_id = integer(), frequency = numeric(), R0_value = numeric())
    R0_history <- data.frame(day = integer(), effective_R0 = numeric(), dominant_variant = integer(), number_variants = integer())

    calib <- variant_calibration_values(variant_emergence_calibration)

    draw_mutation_effect <- function() {
      mutation_class <- sample(
        c("Deleterious", "Neutral", "Small beneficial", "Moderate beneficial", "Large beneficial"),
        size = 1,
        prob = c(0.70, 0.24, 0.05, 0.009, 0.001)
      )
      effect <- switch(
        mutation_class,
        "Deleterious" = -runif(1, min = 0.005, max = 0.15),
        "Neutral" = rnorm(1, mean = 0, sd = 0.0015),
        "Small beneficial" = runif(1, min = 0.005, max = 0.035),
        "Moderate beneficial" = runif(1, min = 0.035, max = 0.10),
        "Large beneficial" = runif(1, min = 0.10, max = 0.20)
      )
      list(class = mutation_class, effect = effect)
    }

    transmission_time <- 0
    mobility_time <- 0
    state_update_time <- 0
    history_time <- 0

    for (i in seq_along(t)) {
      day <- t[i]
      if (!is.null(progress_callback) && (day == 0 || day %% 10 == 0 || day == days)) {
        progress_callback(day, days)
      }

      imported_cases_today <- setNames(rep(0, n_countries), countries)
      expected_imports_today <- setNames(rep(0, n_countries), countries)
      containment_today <- containment_multipliers_for_day(containment_schedule, day)
      transmission_multiplier_by_country <- containment_transmission_vector_for_day(containment_schedule, day, countries)

      active_by_country_age <- apply(I_arr, c(1, 2), sum)
      exposed_by_country_age <- if (isTRUE(use_seird)) apply(E_arr, c(1, 2), sum) else active_by_country_age * 0
      E_country <- keep_country_names(rowSums(exposed_by_country_age), countries)
      I_country <- keep_country_names(rowSums(active_by_country_age), countries)
      pressure_by_country_age <- matrix(0, nrow = n_countries, ncol = n_age, dimnames = list(countries, age_groups))
      for (a in seq_len(n_age)) {
        age_time_matrix <- matrix(I_arr[, a, ], nrow = n_countries, ncol = infectious_duration)
        pressure_by_country_age[, a] <- as.numeric(age_time_matrix %*% infectivity_kernel)
      }
      weighted_pressure_by_country_age <- sweep(pressure_by_country_age, 2, age_params$contact_weight, "*")
      infectious_pressure <- keep_country_names(rowSums(weighted_pressure_by_country_age), countries)

      if (day > 0) {
        # Population-level dynamic replacement among circulating variants.
        variant_ids_chr <- as.character(variants_emerged$variant_id)
        variant_freq <- variant_freq[variant_ids_chr]
        variant_freq[is.na(variant_freq)] <- 0
        names(variant_freq) <- variant_ids_chr
        if (sum(variant_freq) <= 0) variant_freq["1"] <- 1
        variant_freq <- variant_freq / sum(variant_freq)

        if (length(variant_freq) > 1) {
          R0s <- variants_emerged$R0_value
          R0_mean <- sum(variant_freq * R0s)
          if (is.finite(R0_mean) && R0_mean > 0) {
            relative_growth <- log(pmax(R0s, 1e-12) / R0_mean)
            relative_growth <- pmax(-1.2, pmin(1.2, relative_growth))
            variant_freq <- variant_freq * exp(0.85 * relative_growth)
            variant_freq <- variant_freq / sum(variant_freq)
          }
        }
        current_R0_base <- sum(variant_freq * variants_emerged$R0_value)

        tm0 <- Sys.time()
        S_country <- keep_country_names(rowSums(S_age), countries)
        current_R0_country <- current_R0_base * transmission_multiplier_by_country
        current_R0 <- weighted.mean(current_R0_country, country_populations)
        country_pop_safe <- pmax(country_populations, 1)

        local_new_total <- current_R0_country * S_country * infectious_pressure / country_pop_safe
        local_new_total <- keep_country_names(local_new_total, countries)
        local_new_total[!is.finite(local_new_total)] <- 0
        local_new_total <- pmin(local_new_total, S_country)
        local_new_total <- keep_country_names(local_new_total, countries)

        destination_weights <- sweep(S_age, 2, age_params$susceptibility_weight, "*")
        destination_weights[!is.finite(destination_weights)] <- 0
        row_weight_sum <- rowSums(destination_weights)
        local_new_age <- destination_weights * 0
        valid_rows <- row_weight_sum > 0
        local_new_age[valid_rows, ] <- sweep(destination_weights[valid_rows, , drop = FALSE], 1, row_weight_sum[valid_rows], "/") * local_new_total[valid_rows]
        transmission_time <- transmission_time + as.numeric(difftime(Sys.time(), tm0, units = "secs"))

        tm0 <- Sys.time()
        available_after_local_age <- pmax(S_age - local_new_age, 0)
        available_after_local <- keep_country_names(rowSums(available_after_local_age), countries)
        Rt_destination <- current_R0_country * available_after_local / country_pop_safe
        Rt_destination <- keep_country_names(Rt_destination, countries)
        Rt_destination[!is.finite(Rt_destination)] <- 0

        if (sum(I_country > 0) > 0 && import_establishment_probability > 0) {
          origin_age_time <- apply(I_arr, c(1, 3), sum)
          origin_age_prevalence <- sweep(origin_age_time, 1, country_pop_safe, "/")
          origin_age_prevalence[!is.finite(origin_age_prevalence)] <- 0

          p_seed_by_destination_age <- t(vapply(
            Rt_destination,
            import_seed_probability,
            remaining_infectivity = remaining_infectivity,
            opportunity_modifier = import_establishment_probability,
            FUN.VALUE = numeric(length(remaining_infectivity))
          ))
          p_seed_by_destination_age[available_after_local <= 0, ] <- 0
          p_seed_by_destination_age[!is.finite(p_seed_by_destination_age)] <- 0

          route_seed_factor <- origin_age_prevalence %*% t(p_seed_by_destination_age)
          passenger_matrix_for_imports <- apply_containment_to_passenger_matrix(passenger_matrix_daily, containment_schedule, day, countries)
          diag(passenger_matrix_for_imports) <- 0
          passenger_matrix_for_imports[!is.finite(passenger_matrix_for_imports)] <- 0

          expected_imports_today <- colSums(passenger_matrix_for_imports * route_seed_factor, na.rm = TRUE)
          expected_imports_today <- keep_country_names(expected_imports_today, countries)
          expected_imports_today[!is.finite(expected_imports_today)] <- 0
          expected_imports_today <- pmin(expected_imports_today, available_after_local)
          imported_cases_today <- expected_imports_today
          imported_cases_today[imported_cases_today < 1e-8] <- 0
        }

        import_destination_weights <- sweep(available_after_local_age, 2, age_params$susceptibility_weight, "*")
        import_row_sum <- rowSums(import_destination_weights)
        imported_age <- import_destination_weights * 0
        valid_import_rows <- import_row_sum > 0
        imported_age[valid_import_rows, ] <- sweep(import_destination_weights[valid_import_rows, , drop = FALSE], 1, import_row_sum[valid_import_rows], "/") * imported_cases_today[valid_import_rows]
        mobility_time <- mobility_time + as.numeric(difftime(Sys.time(), tm0, units = "secs"))

        tm0 <- Sys.time()
        new_age_cases <- local_new_age + imported_age
        new_age_cases <- pmin(new_age_cases, S_age)
        daily_new_infections_total <- sum(new_age_cases, na.rm = TRUE)

        leaving_by_age <- I_arr[, , infectious_duration, drop = FALSE][, , 1]
        deaths_age <- sweep(leaving_by_age, 2, age_params$cfr, "*")
        recoveries_age <- leaving_by_age - deaths_age

        if (isTRUE(use_seird)) {
          new_exposed_age <- new_age_cases
          new_active_age <- E_arr[, , exposed_duration, drop = FALSE][, , 1]

          S_age <- pmax(S_age - new_exposed_age, 0)
          R_age <- R_age + recoveries_age
          D_age <- D_age + deaths_age

          if (exposed_duration == 1) {
            E_arr[, , 1] <- new_exposed_age
          } else {
            E_arr[, , 2:exposed_duration] <- E_arr[, , 1:(exposed_duration - 1), drop = FALSE]
            E_arr[, , 1] <- new_exposed_age
          }

          if (infectious_duration == 1) {
            I_arr[, , 1] <- new_active_age
          } else {
            I_arr[, , 2:infectious_duration] <- I_arr[, , 1:(infectious_duration - 1), drop = FALSE]
            I_arr[, , 1] <- new_active_age
          }
        } else {
          S_age <- pmax(S_age - new_age_cases, 0)
          R_age <- R_age + recoveries_age
          D_age <- D_age + deaths_age

          if (infectious_duration == 1) {
            I_arr[, , 1] <- new_age_cases
          } else {
            I_arr[, , 2:infectious_duration] <- I_arr[, , 1:(infectious_duration - 1), drop = FALSE]
            I_arr[, , 1] <- new_age_cases
          }
        }

        newly_reached <- countries[imported_cases_today > 0 & is.na(first_reached_day)]
        if (length(newly_reached) > 0) first_reached_day[newly_reached] <- day

        # Macroscopic stochastic dynamic events, based on incident activity.
        I_total_after_update <- sum(I_arr, na.rm = TRUE)
        if (enable_evolution && I_total_after_update > 0 && daily_new_infections_total > 0) {
          raw_mutational_opportunities <- daily_new_infections_total * mutation_rate_per_replication * effective_mutation_targets
          candidate_lambda <- raw_mutational_opportunities * calib$observable_fraction
          candidate_lambda <- max(0, min(candidate_lambda, 25))
          num_candidates <- rpois(1, lambda = candidate_lambda)

          if (num_candidates > 0) {
            origin_prob <- pmax(rowSums(new_age_cases), 0)
            origin_prob[!is.finite(origin_prob)] <- 0
            if (sum(origin_prob) <= 0) origin_prob <- pmax(I_country, 0)
            origin_prob[!is.finite(origin_prob)] <- 0
            if (sum(origin_prob) <= 0) {
              origin_prob <- setNames(rep(1 / length(countries), length(countries)), countries)
            } else {
              origin_prob <- origin_prob / sum(origin_prob)
            }

            for (cand in seq_len(num_candidates)) {
              mut <- draw_mutation_effect()
              raw_effect <- mut$effect
              origin_country <- sample(countries, size = 1, prob = origin_prob)

              dominant_id <- as.integer(names(which.max(variant_freq)))
              parent_R0 <- variants_emerged$R0_value[variants_emerged$variant_id == dominant_id]
              if (length(parent_R0) == 0 || !is.finite(parent_R0)) parent_R0 <- base_R0

              adaptive_space_multiplier <- 1
              if (isTRUE(dynamic_adaptive_saturation) && is.finite(raw_effect) && raw_effect > 0 && is.finite(R0_cap) && is.finite(base_R0) && R0_cap > base_R0) {
                current_reference_R0 <- if (exists("current_R0_base") && is.finite(current_R0_base)) current_R0_base else parent_R0
                current_reference_R0 <- max(base_R0, min(R0_cap, current_reference_R0))
                adaptive_space_multiplier <- (R0_cap - current_reference_R0) / (R0_cap - base_R0)
                adaptive_space_multiplier <- max(0, min(1, adaptive_space_multiplier))
                adaptive_space_multiplier <- adaptive_space_multiplier ^ max(0.01, dynamic_saturation_exponent)
              }
              expressed_effect <- ifelse(is.finite(raw_effect) && raw_effect > 0, raw_effect * adaptive_space_multiplier, raw_effect)

              candidate_R0 <- max(0.01 * base_R0, min(R0_cap, parent_R0 * (1 + expressed_effect)))
              candidate_beta <- candidate_R0 / infectious_duration
              realized_advantage <- (candidate_R0 / parent_R0) - 1

              establishment_probability <- 0
              established <- FALSE
              if (realized_advantage > 0.003) {
                prevalence_factor <- (I_total_after_update / (I_total_after_update + 1e6)) ^ 0.35
                establishment_probability <- calib$establishment_multiplier * 2.2 * realized_advantage * prevalence_factor
                establishment_probability <- max(0, min(0.35, establishment_probability))
                if (runif(1) < establishment_probability) {
                  established <- TRUE
                  variant_count <- variant_count + 1
                  intro_frequency <- min(0.03, max(0.001, 50 / max(I_total_after_update, 1)))
                  variants_emerged <- rbind(
                    variants_emerged,
                    data.frame(
                      day = day,
                      variant_id = variant_count,
                      parent_variant_id = dominant_id,
                      origin_country = origin_country,
                      beta_value = candidate_beta,
                      R0_value = candidate_R0,
                      fitness_advantage = realized_advantage * 100,
                      establishment_probability = establishment_probability,
                      mutation_class = mut$class,
                      stringsAsFactors = FALSE
                    )
                  )
                  variant_freq <- variant_freq * (1 - intro_frequency)
                  variant_freq[as.character(variant_count)] <- intro_frequency
                  variant_freq <- variant_freq / sum(variant_freq)
                }
              }

              candidate_mutations <- rbind(
                candidate_mutations,
                data.frame(
                  day = day,
                  origin_country = origin_country,
                  mutation_class = mut$class,
                  raw_fitness_effect = raw_effect * 100,
                  expressed_fitness_effect = expressed_effect * 100,
                  adaptive_space_multiplier = adaptive_space_multiplier,
                  establishment_probability = establishment_probability,
                  established = established,
                  candidate_R0 = candidate_R0,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
        }

        state_update_time <- state_update_time + as.numeric(difftime(Sys.time(), tm0, units = "secs"))
      }

      tm0 <- Sys.time()
      active_by_country_age <- apply(I_arr, c(1, 2), sum)
      exposed_by_country_age <- if (isTRUE(use_seird)) apply(E_arr, c(1, 2), sum) else active_by_country_age * 0
      E_country <- keep_country_names(rowSums(exposed_by_country_age), countries)
      I_country <- keep_country_names(rowSums(active_by_country_age), countries)
      pressure_by_country_age <- matrix(0, nrow = n_countries, ncol = n_age, dimnames = list(countries, age_groups))
      for (a in seq_len(n_age)) {
        age_time_matrix <- matrix(I_arr[, a, ], nrow = n_countries, ncol = infectious_duration)
        pressure_by_country_age[, a] <- as.numeric(age_time_matrix %*% infectivity_kernel)
      }
      infectious_pressure <- keep_country_names(rowSums(sweep(pressure_by_country_age, 2, age_params$contact_weight, "*")), countries)

      result$S[i] <- sum(S_age)
      result$E[i] <- if (isTRUE(use_seird)) sum(E_country) else 0
      result$I[i] <- sum(I_country)
      result$R[i] <- sum(R_age)
      result$D[i] <- sum(D_age)
      exposed_country_history[i, ] <- E_country
      country_history[i, ] <- I_country
      death_country_history[i, ] <- rowSums(D_age)
      import_history[i, ] <- imported_cases_today
      expected_import_history[i, ] <- expected_imports_today
      infectious_pressure_history[i, ] <- infectious_pressure

      variant_ids_chr <- as.character(variants_emerged$variant_id)
      variant_freq <- variant_freq[variant_ids_chr]
      variant_freq[is.na(variant_freq)] <- 0
      names(variant_freq) <- variant_ids_chr
      if (sum(variant_freq) <= 0) variant_freq["1"] <- 1
      variant_freq <- variant_freq / sum(variant_freq)

      for (vid in variants_emerged$variant_id) {
        vid_chr <- as.character(vid)
        this_freq <- ifelse(vid_chr %in% names(variant_freq), variant_freq[vid_chr], 0)
        this_R0 <- variants_emerged$R0_value[variants_emerged$variant_id == vid][1]
        variant_frequency_history <- rbind(
          variant_frequency_history,
          data.frame(day = day, variant_id = vid, frequency = as.numeric(this_freq), R0_value = this_R0)
        )
      }

      R0_history <- rbind(
        R0_history,
        data.frame(day = day, effective_R0 = if (exists("current_R0")) current_R0 else base_R0, dominant_variant = as.integer(names(which.max(variant_freq))), number_variants = nrow(variants_emerged))
      )

      age_snapshot <- data.frame(
        day = day,
        age_group = age_groups,
        exposed = colSums(exposed_by_country_age),
        active = colSums(active_by_country_age),
        recovered = colSums(R_age),
        deaths = colSums(D_age),
        stringsAsFactors = FALSE
      )
      age_history <- rbind(age_history, age_snapshot)
      history_time <- history_time + as.numeric(difftime(Sys.time(), tm0, units = "secs"))
    }

    age_summary <- data.frame(
      age_group = age_groups,
      population = colSums(sweep(country_age_distribution, 1, country_populations, "*")),
      exposed_final = if ("exposed" %in% names(age_history)) as.numeric(tail(subset(age_history, day == days)$exposed, n_age)) else rep(0, n_age),
      active_final = as.numeric(tail(subset(age_history, day == days)$active, n_age)),
      cumulative_resolved = colSums(R_age + D_age),
      deaths = colSums(D_age),
      cfr = age_params$cfr,
      death_share = if (sum(D_age) > 0) colSums(D_age) / sum(D_age) else rep(0, n_age),
      stringsAsFactors = FALSE
    )

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cost_profile <- list(
      model_structure = paste0(tolower(disease_compartment_model), "_", if (isTRUE(neutral_age_weights)) "age_adjusted_step2_cfr_neutral" else "age_adjusted_step2_cfr_differential"),
      compartment_model = disease_compartment_model,
      exposed_period_days = ifelse(isTRUE(use_seird), exposed_duration, 0),
      dynamic_module = ifelse(isTRUE(enable_evolution), "active", "off"),
      elapsed_seconds = sprintf("%.2f", elapsed),
      days = days,
      countries = n_countries,
      age_groups = n_age,
      age_parameter_mode = if (isTRUE(neutral_age_weights)) "neutral" else "differential_cfr",
      age_distribution_mode = age_distribution_mode,
      country_age_distribution_source = age_distribution_source,
      country_age_distribution_file = age_distribution_file,
      country_age_distribution_searched_dirs = attr(country_age_distribution, "searched_dirs"),
      country_polygon_source = WORLD_COUNTRY_POLYGONS$source,
      country_polygon_file = WORLD_COUNTRY_POLYGONS$file,
      age_cfr_scale = age_cfr_scale,
      target_weighted_cfr = sprintf("%.4f", unique(age_params$target_weighted_cfr)[1]),
      achieved_weighted_cfr = sprintf("%.4f", unique(age_params$achieved_weighted_cfr)[1]),
      cfr_saturated_groups = as.character(unique(age_params$cfr_saturated_groups)[1]),
      min_age_cfr = sprintf("%.4f", unique(age_params$min_age_cfr)[1]),
      max_age_cfr = sprintf("%.4f", unique(age_params$max_age_cfr)[1]),
      cfr_warning = as.character(unique(age_params$cfr_warning)[1]),
      active_cells = n_countries * n_age * infectious_duration,
      infectious_window_days = infectious_duration,
      transmission_seconds = sprintf("%.2f", transmission_time),
      mobility_seconds = sprintf("%.2f", mobility_time),
      state_update_seconds = sprintf("%.2f", state_update_time),
      history_seconds = sprintf("%.2f", history_time),
      starting_country_mode = NA,
      rds_reference = "selected_fixed_reference_file_set_after_run",
      containment_enabled = ifelse(is.null(containment_schedule), FALSE, containment_schedule$enabled),
      containment_preset = ifelse(is.null(containment_schedule), "none", containment_schedule$preset),
      containment_start_day = ifelse(is.null(containment_schedule), NA, containment_schedule$start_day),
      containment_end_day = ifelse(is.null(containment_schedule), NA, containment_schedule$end_day),
      containment_transmission_reduction_percent = ifelse(is.null(containment_schedule), 0, containment_schedule$transmission_reduction_percent),
      containment_mobility_reduction_percent = ifelse(is.null(containment_schedule), 0, containment_schedule$mobility_reduction_percent),
      containment_transmission_multiplier = ifelse(is.null(containment_schedule), 1, containment_schedule$transmission_multiplier),
      containment_mobility_multiplier = ifelse(is.null(containment_schedule), 1, containment_schedule$mobility_multiplier),
      containment_geographic_scope = ifelse(is.null(containment_schedule), "global", containment_schedule$geographic_scope),
      containment_affected_continents = ifelse(is.null(containment_schedule), "", paste(containment_schedule$affected_continents, collapse = ";")),
      containment_affected_country_count = ifelse(is.null(containment_schedule), 0, containment_schedule$affected_country_count),
      containment_affected_countries = ifelse(is.null(containment_schedule), "", paste(containment_schedule$affected_countries, collapse = ";")),
      containment_mobility_application_rule = ifelse(is.null(containment_schedule), "none", containment_schedule$mobility_application_rule)
    )

    list(
      data = result,
      variants_emerged = variants_emerged,
      candidate_mutations = candidate_mutations,
      variant_frequency_history = variant_frequency_history,
      R0_history = R0_history,
      country_history = country_history,
      death_country_history = death_country_history,
      new_country_import_history = import_history,
      expected_import_history = expected_import_history,
      infectious_pressure_history = infectious_pressure_history,
      first_reached_day = first_reached_day,
      passenger_matrix_annual = passenger_matrix_annual,
      infectivity_kernel = infectivity_kernel,
      remaining_infectivity = remaining_infectivity,
      countries = countries,
      final_R0 = round(tail(R0_history$effective_R0, 1), 2),
      total_variants = max(0, nrow(variants_emerged) - 1),
      age_parameters = age_params,
      country_age_distribution = country_age_distribution,
      age_history = age_history,
      age_summary = age_summary,
      model_structure = paste0(tolower(disease_compartment_model), "_", if (isTRUE(neutral_age_weights)) "age_adjusted_step2_cfr_neutral" else "age_adjusted_step2_cfr_differential"),
      compartment_model = disease_compartment_model,
      exposed_period_days = ifelse(isTRUE(use_seird), exposed_duration, 0),
      cost_profile = cost_profile
    )
  }


  # Fixed COVID-19 Omicron references loaded from precomputed RDS caches.
  # The basic RDS is mandatory. The age-adjusted RDS is optional and is used when
  # available and selected in the UI. Neither comparator is recomputed when the
  # user clicks Calculate simulation.
  locate_reference_rds <- function(filename) {
    candidate_dirs <- unique(c(
      tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)), error = function(e) NA_character_),
      getwd(),
      Sys.getenv("EPIDEM_REFERENCE_DIR", unset = NA_character_)
    ))
    candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)]
    direct <- file.path(candidate_dirs, filename)
    hit <- direct[file.exists(direct)]
    if (length(hit) > 0) return(normalizePath(hit[1], winslash = "/", mustWork = FALSE))
    recursive_hits <- unlist(lapply(candidate_dirs, function(d) {
      if (!dir.exists(d)) return(character())
      list.files(d, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"), recursive = TRUE, full.names = TRUE)
    }), use.names = FALSE)
    if (length(recursive_hits) > 0) return(normalizePath(recursive_hits[1], winslash = "/", mustWork = FALSE))
    NA_character_
  }

  required_reference_elements <- c(
    "data", "R0_history", "country_history", "death_country_history",
    "new_country_import_history", "expected_import_history",
    "infectious_pressure_history", "variants_emerged",
    "variant_frequency_history", "first_reached_day",
    "passenger_matrix_annual"
  )

  load_fixed_reference_cache <- function(path, reference_type = "basic") {
    ref <- readRDS(path)
    missing_reference_elements <- setdiff(required_reference_elements, names(ref))
    if (length(missing_reference_elements) > 0) {
      stop(
        paste0(
          "The precomputed COVID-19 Omicron reference RDS is incomplete. Missing elements: ",
          paste(missing_reference_elements, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    ref$metrics <- calculate_metrics(ref$data, WORLD_POPULATION)
    if (is.null(ref$R0)) ref$R0 <- 4.25
    if (is.null(ref$final_R0)) ref$final_R0 <- ref$R0
    if (is.null(ref$total_variants)) ref$total_variants <- max(0, nrow(ref$variants_emerged) - 1)
    ref$color <- "#27AE60"
    ref$reference_type <- reference_type
    ref$reference_file <- path
    ref
  }

  fixed_covid_reference_basic_file <- locate_reference_rds("fixed_covid_omicron_reference_sir.rds")
  if (is.na(fixed_covid_reference_basic_file) || !file.exists(fixed_covid_reference_basic_file)) {
    stop(
      paste0(
        "Missing mandatory precomputed COVID-19 Omicron reference file: fixed_covid_omicron_reference_sir.rds. ",
        "Generate it with generate_fixed_covid_omicron_reference_sir.R and place it in the app folder."
      ),
      call. = FALSE
    )
  }
  fixed_covid_reference_basic_cache <- load_fixed_reference_cache(fixed_covid_reference_basic_file, "basic")

  fixed_covid_reference_age_file <- locate_reference_rds("fixed_covid_omicron_reference_age_adjusted.rds")
  fixed_covid_reference_age_cache <- NULL
  if (!is.na(fixed_covid_reference_age_file) && file.exists(fixed_covid_reference_age_file)) {
    fixed_covid_reference_age_cache <- load_fixed_reference_cache(fixed_covid_reference_age_file, "age_adjusted_sird")
    fixed_covid_reference_age_cache$compartment_model <- "SIRD"
  }

  fixed_covid_reference_age_seird_file <- locate_reference_rds("fixed_covid_omicron_reference_age_adjusted_seird.rds")
  fixed_covid_reference_age_seird_cache <- NULL
  if (!is.na(fixed_covid_reference_age_seird_file) && file.exists(fixed_covid_reference_age_seird_file)) {
    fixed_covid_reference_age_seird_cache <- load_fixed_reference_cache(fixed_covid_reference_age_seird_file, "age_adjusted_seird")
    fixed_covid_reference_age_seird_cache$compartment_model <- "SEIRD"
  }

  active_compartment_model_for_reference <- reactive({
    guided_mode <- identical(input$active_scenario_mode, "guided")
    if (isTRUE(guided_mode)) {
      "SEIRD"
    } else if (!is.null(input$disease_compartment_model) && input$disease_compartment_model %in% c("SIRD", "SEIRD")) {
      input$disease_compartment_model
    } else {
      "SEIRD"
    }
  })

  selected_fixed_reference_cache <- reactive({
    requested <- input$reference_comparator_type
    if (is.null(requested) || identical(requested, "auto")) {
      active_model <- active_compartment_model_for_reference()
      if (identical(active_model, "SEIRD") && !is.null(fixed_covid_reference_age_seird_cache)) {
        return(fixed_covid_reference_age_seird_cache)
      }
      if (!is.null(fixed_covid_reference_age_cache)) {
        return(fixed_covid_reference_age_cache)
      }
      return(fixed_covid_reference_basic_cache)
    }

    if (identical(requested, "age_adjusted_seird") && !is.null(fixed_covid_reference_age_seird_cache)) {
      return(fixed_covid_reference_age_seird_cache)
    }
    if (identical(requested, "age_adjusted") && !is.null(fixed_covid_reference_age_cache)) {
      return(fixed_covid_reference_age_cache)
    }
    fixed_covid_reference_basic_cache
  })

  selected_fixed_reference_file <- reactive({
    ref <- selected_fixed_reference_cache()
    basename(ref$reference_file)
  })

  selected_fixed_reference_type <- reactive({
    ref <- selected_fixed_reference_cache()
    ref$reference_type
  })

  selected_fixed_reference_fallback_note <- reactive({
    requested <- input$reference_comparator_type
    active_model <- active_compartment_model_for_reference()

    if ((is.null(requested) || identical(requested, "auto")) &&
        identical(active_model, "SEIRD") &&
        is.null(fixed_covid_reference_age_seird_cache)) {
      if (!is.null(fixed_covid_reference_age_cache)) {
        return("SEIRD age-adjusted comparator RDS not found; using age-adjusted SIRD comparator. Generate fixed_covid_omicron_reference_age_adjusted_seird.rds for strict like-for-like comparison.")
      }
      return("SEIRD age-adjusted comparator RDS not found; using basic SIRD comparator. Generate fixed_covid_omicron_reference_age_adjusted_seird.rds for strict like-for-like comparison.")
    }

    if (identical(requested, "age_adjusted_seird") && is.null(fixed_covid_reference_age_seird_cache)) {
      return("Requested SEIRD age-adjusted comparator RDS not found; using fallback comparator.")
    }

    if (identical(requested, "age_adjusted") && is.null(fixed_covid_reference_age_cache)) {
      return("Age-adjusted SIRD comparator RDS not found; using basic comparator instead.")
    }

    ""
  })

  slice_fixed_reference <- function(ref, days) {
    idx <- seq_len(min(days + 1, nrow(ref$data)))
    out <- ref
    out$data <- ref$data[idx, , drop = FALSE]
    out$metrics <- calculate_metrics(out$data, WORLD_POPULATION)
    out$variant_frequency_history <- ref$variant_frequency_history[ref$variant_frequency_history$day <= days, , drop = FALSE]
    out$R0_history <- ref$R0_history[ref$R0_history$day <= days, , drop = FALSE]
    out$country_history <- ref$country_history[idx, , drop = FALSE]
    if (!is.null(ref$exposed_country_history)) {
      out$exposed_country_history <- ref$exposed_country_history[idx, , drop = FALSE]
    }
    out$death_country_history <- ref$death_country_history[idx, , drop = FALSE]
    out$new_country_import_history <- ref$new_country_import_history[idx, , drop = FALSE]
    out$expected_import_history <- ref$expected_import_history[idx, , drop = FALSE]
    out$infectious_pressure_history <- ref$infectious_pressure_history[idx, , drop = FALSE]
    out
  }

  observeEvent(input$run_simulation, {
    animation_running(FALSE)
    updateSliderInput(session, "time_slider", value = 0)
    updateTabsetPanel(session, "main_tabs", selected = "World map")
    session$sendCustomMessage("scrollToMainMap", TRUE)
    run_notification_id <- showNotification("Running simulation...", type = "message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(run_notification_id), add = TRUE)

    N <- WORLD_POPULATION
    I0 <- input$initial_infected
    days <- input$simulation_days
    guided_mode <- identical(isolate(input$active_scenario_mode), "guided")
    raw_optional_seed <- parse_optional_seed(input$stochastic_seed)
    optional_seed <- if (guided_mode) guided_internal_seed() else raw_optional_seed
    current_country <- isolate(input$starting_country)
    starting_country_mode <- if (guided_mode) "manual" else isolate(input$starting_country_mode)
    available_countries <- setdiff(COUNTRIES_LIST, current_country)
    if (identical(starting_country_mode, "manual")) {
      selected_starting_country <- current_country
    } else if (identical(starting_country_mode, "seeded_stable") && !is.null(optional_seed)) {
      set.seed(optional_seed)
      selected_starting_country <- sample(COUNTRIES_LIST, 1)
    } else if (identical(starting_country_mode, "seeded_stable") && is.null(optional_seed)) {
      selected_starting_country <- current_country
    } else {
      selected_starting_country <- sample(available_countries, 1)
    }
    updateSelectInput(session, "starting_country", selected = selected_starting_country)
    updateSliderInput(session, "time_slider", min = 0, max = days, value = 0, step = 1)

    containment_schedule <- make_containment_schedule(
      enabled = ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))),
      preset = input$containment_preset,
      start_day = input$containment_start_day,
      end_day = input$containment_end_day,
      custom_transmission_reduction = input$containment_transmission_reduction,
      custom_mobility_reduction = input$containment_mobility_reduction,
      label = input$containment_label,
      geographic_scope = input$containment_geographic_scope,
      affected_continents = input$containment_affected_continents,
      affected_countries = input$containment_affected_countries
    )

    dynamic_config <- get_dynamic_config()
    disease_model <- if (isTRUE(guided_mode)) "SEIRD" else if (!is.null(input$disease_compartment_model)) input$disease_compartment_model else "SEIRD"
    exposed_days <- if (!is.null(input$exposed_period_days)) input$exposed_period_days else 4
    if (!identical(input$model_structure, "age_adjusted")) {
      disease_model <- "SIRD"
      exposed_days <- 0
    }

    computation_status(sprintf("Preparing calculation: %s/%s mode, %s days, %s countries, starting country %s, active window %s days. Dynamic scenario: %s.", input$model_structure, disease_model, days, length(COUNTRIES_LIST), selected_starting_country, input$infectious_period_days, dynamic_config$scenario_label))

    run_event_start <- Sys.time()
    new_results <- withProgress(message = "Calculating user simulation", value = 0, {
      incProgress(0.10, detail = "Initialising compartments and passenger matrix")
      if (identical(input$model_structure, "age_adjusted")) {
        result <- run_simulation_age_adjusted(
          R0_target = input$R0,
          infectious_period_days = input$infectious_period_days,
          mortality_rate = input$mortality_rate / 100,
          N = N,
          I0 = I0,
          days = days,
          mutation_rate_per_replication = dynamic_config$rate,
          effective_mutation_targets = dynamic_config$targets,
          variant_emergence_calibration = dynamic_config$calibration,
          maximum_variant_R0_multiplier = dynamic_config$max_multiplier,
          enable_evolution = isTRUE(dynamic_config$enabled),
          starting_country = selected_starting_country,
          air_travel_scenario = input$air_travel_scenario,
          import_establishment_probability = input$import_establishment_probability,
          infectiousness_profile = input$infectiousness_profile,
          rng_seed = optional_seed,
          progress_callback = function(day, total_days) {
            incProgress(0.70 * day / max(total_days, 1), detail = sprintf("Simulating age-adjusted day %s of %s", day, total_days))
          },
          neutral_age_weights = !identical(input$age_parameter_mode, "differential_cfr"),
          age_cfr_scale = input$age_cfr_scale,
          containment_schedule = containment_schedule,
          dynamic_adaptive_saturation = isTRUE(dynamic_config$adaptive_saturation),
          dynamic_saturation_exponent = dynamic_config$saturation_exponent,
          age_distribution_mode = if (!is.null(input$age_distribution_mode)) input$age_distribution_mode else "country_specific",
          disease_compartment_model = disease_model,
          exposed_period_days = exposed_days
        )
      } else {
        result <- run_simulation(
          R0_target = input$R0,
          infectious_period_days = input$infectious_period_days,
          mortality_rate = input$mortality_rate / 100,
          N = N,
          I0 = I0,
          days = days,
          mutation_rate_per_replication = dynamic_config$rate,
          effective_mutation_targets = dynamic_config$targets,
          variant_emergence_calibration = dynamic_config$calibration,
          maximum_variant_R0_multiplier = dynamic_config$max_multiplier,
          starting_country = selected_starting_country,
          air_travel_scenario = input$air_travel_scenario,
          import_establishment_probability = input$import_establishment_probability,
          infectiousness_profile = input$infectiousness_profile,
          enable_evolution = isTRUE(dynamic_config$enabled),
          rng_seed = optional_seed,
          progress_callback = function(day, total_days) {
            incProgress(0.70 * day / max(total_days, 1), detail = sprintf("Simulating day %s of %s", day, total_days))
          },
          containment_schedule = containment_schedule,
          dynamic_adaptive_saturation = isTRUE(dynamic_config$adaptive_saturation),
          dynamic_saturation_exponent = dynamic_config$saturation_exponent
        )
        result$model_structure <- "basic"
        result$cost_profile <- list(
          model_structure = "basic",
          elapsed_seconds = "captured_by_run_event",
          days = days,
          countries = length(COUNTRIES_LIST),
          age_groups = 0,
          active_cells = length(COUNTRIES_LIST) * max(1, as.integer(round(input$infectious_period_days))),
          infectious_window_days = max(1, as.integer(round(input$infectious_period_days))),
          dynamic_module = ifelse(isTRUE(dynamic_config$enabled), "active", "off"),
          dynamic_candidate_events = NA,
          dynamic_established_events = NA,
          dynamic_initial_R0 = NA,
          dynamic_final_effective_R0 = NA,
          dynamic_runtime_overhead_seconds = ifelse(isTRUE(dynamic_config$enabled), "not_paired_in_current_run", "0.00"),
          starting_country_mode = input$starting_country_mode,
          cfr_saturated_groups = NA,
          min_age_cfr = NA,
          max_age_cfr = NA,
          cfr_warning = NA,
          rds_reference = selected_fixed_reference_file()
        )
      }
      incProgress(0.80, detail = "Preparing COVID-19 Omicron reference and outputs")
      result
    })
    run_event_elapsed <- as.numeric(difftime(Sys.time(), run_event_start, units = "secs"))
    if (is.null(new_results$cost_profile)) new_results$cost_profile <- list()
    new_results$starting_country_used <- selected_starting_country
    new_results$cost_profile$run_event_elapsed_seconds <- sprintf("%.2f", run_event_elapsed)
    new_results$cost_profile$starting_country_mode <- starting_country_mode
    new_results$cost_profile$seed_visibility_mode <- ifelse(guided_mode, "hidden_guided_seed", "advanced_user_seed")
    new_results$cost_profile$scenario_internal_seed_for_json <- ifelse(is.null(optional_seed), NA, as.character(optional_seed))
    new_results$cost_profile$starting_country_selection_source <- ifelse(guided_mode, guided_country_selection_source(), starting_country_mode)
    new_results$cost_profile$guided_random_country_clicks <- ifelse(guided_mode && identical(guided_country_selection_source(), "basic_random_button"), as.character(guided_random_country_clicks()), NA)
    new_results$cost_profile$effective_seed_used <- ifelse(guided_mode, "hidden_guided_seed_stored_for_future_json", ifelse(is.null(optional_seed), "blank_random", as.character(optional_seed)))
    new_results$cost_profile$reproducibility_warning <- ifelse(guided_mode, "none", ifelse(identical(starting_country_mode, "seeded_stable") && is.null(optional_seed), "seeded_stable_without_seed_not_reproducible", "none"))
    new_results$cost_profile$rds_reference <- selected_fixed_reference_file()
    new_results$cost_profile$reference_comparator_type <- selected_fixed_reference_type()
    new_results$cost_profile$reference_comparator_fallback_note <- selected_fixed_reference_fallback_note()
    new_results$cost_profile$active_compartment_model <- if (!is.null(new_results$compartment_model)) new_results$compartment_model else disease_model
    new_results$cost_profile$active_exposed_period_days <- if (!is.null(new_results$exposed_period_days)) new_results$exposed_period_days else ifelse(identical(disease_model, "SEIRD"), exposed_days, 0)
    selected_ref_model <- if (!is.null(selected_fixed_reference_cache()$compartment_model)) selected_fixed_reference_cache()$compartment_model else "SIRD"
    new_results$cost_profile$fixed_comparator_compartment_model <- selected_ref_model
    new_results$cost_profile$comparator_structure_warning <- ifelse(
      identical(new_results$cost_profile$active_compartment_model, selected_ref_model),
      "none",
      paste0("active_", new_results$cost_profile$active_compartment_model, "_vs_fixed_", selected_ref_model, "_comparator")
    )
    if ("E" %in% names(new_results$data)) {
      exposed_values <- suppressWarnings(as.numeric(new_results$data$E))
      exposed_values[!is.finite(exposed_values)] <- 0
      peak_exposed_idx <- which.max(exposed_values)
      new_results$cost_profile$peak_exposed <- round(exposed_values[peak_exposed_idx])
      new_results$cost_profile$peak_exposed_day <- new_results$data$time[peak_exposed_idx]
      new_results$cost_profile$final_exposed <- round(tail(exposed_values, 1))
    } else {
      new_results$cost_profile$peak_exposed <- 0
      new_results$cost_profile$peak_exposed_day <- NA
      new_results$cost_profile$final_exposed <- 0
    }
    new_results$containment_schedule <- containment_schedule
    new_results$cost_profile$containment_enabled <- containment_schedule$enabled
    new_results$cost_profile$containment_preset <- containment_schedule$preset
    new_results$cost_profile$containment_start_day <- containment_schedule$start_day
    new_results$cost_profile$containment_end_day <- containment_schedule$end_day
    new_results$cost_profile$containment_transmission_reduction_percent <- containment_schedule$transmission_reduction_percent
    new_results$cost_profile$containment_mobility_reduction_percent <- containment_schedule$mobility_reduction_percent
    new_results$cost_profile$containment_transmission_multiplier <- containment_schedule$transmission_multiplier
    new_results$cost_profile$containment_mobility_multiplier <- containment_schedule$mobility_multiplier
    new_results$cost_profile$containment_geographic_scope <- containment_schedule$geographic_scope
    new_results$cost_profile$containment_affected_continents <- paste(containment_schedule$affected_continents, collapse = ";")
    new_results$cost_profile$containment_affected_country_count <- containment_schedule$affected_country_count
    new_results$cost_profile$containment_affected_countries <- paste(containment_schedule$affected_countries, collapse = ";")
    new_results$cost_profile$containment_mobility_application_rule <- containment_schedule$mobility_application_rule
    new_results$cost_profile$dynamic_scenario <- dynamic_config$scenario
    new_results$cost_profile$dynamic_scenario_label <- dynamic_config$scenario_label
    new_results$cost_profile$dynamic_rate <- dynamic_config$rate
    new_results$cost_profile$dynamic_effective_targets <- dynamic_config$targets
    new_results$cost_profile$dynamic_calibration <- dynamic_config$calibration
    new_results$cost_profile$dynamic_max_R0_multiplier <- dynamic_config$max_multiplier

    horizon_diag <- calculate_horizon_diagnostics(new_results, input$mortality_rate / 100, WORLD_POPULATION)
    get_horizon_value <- function(metric_name) {
      val <- horizon_diag$Value[horizon_diag$Metric == metric_name]
      if (length(val) == 0) return(NA)
      val[1]
    }
    new_results$cost_profile$final_active_percent_population <- get_horizon_value("final_active_percent_population")
    new_results$cost_profile$final_active_warning <- get_horizon_value("final_active_warning")
    new_results$cost_profile$projected_additional_deaths_if_active_resolved <- get_horizon_value("projected_additional_deaths_if_active_resolved")
    new_results$cost_profile$projected_final_deaths_if_active_resolved <- get_horizon_value("projected_final_deaths_if_active_resolved")

    dynamic_active <- isTRUE(dynamic_config$enabled)
    dynamic_candidates <- if (!is.null(new_results$candidate_mutations)) nrow(new_results$candidate_mutations) else 0
    dynamic_established <- if (!is.null(new_results$variants_emerged)) max(0, nrow(new_results$variants_emerged) - 1) else 0
    dynamic_initial_R0 <- if (!is.null(new_results$R0_history) && nrow(new_results$R0_history) > 0) {
      suppressWarnings(as.numeric(new_results$R0_history$effective_R0[1]))
    } else {
      suppressWarnings(as.numeric(input$R0))
    }
    dynamic_final_R0 <- if (!is.null(new_results$final_R0)) suppressWarnings(as.numeric(new_results$final_R0)) else NA_real_
    new_results$cost_profile$dynamic_candidate_events <- if (dynamic_active) dynamic_candidates else 0
    new_results$cost_profile$dynamic_established_events <- if (dynamic_active) dynamic_established else 0
    new_results$cost_profile$dynamic_initial_R0 <- if (is.finite(dynamic_initial_R0)) sprintf("%.2f", dynamic_initial_R0) else NA
    new_results$cost_profile$dynamic_final_effective_R0 <- if (is.finite(dynamic_final_R0)) sprintf("%.2f", dynamic_final_R0) else NA
    new_results$cost_profile$dynamic_runtime_overhead_seconds <- if (dynamic_active) "not_paired_in_current_run" else "0.00"

    dynamic_diag <- calculate_dynamic_diagnostics(new_results, input$R0, dynamic_config$max_multiplier)
    get_dynamic_diag_value <- function(metric_name) {
      val <- dynamic_diag$Value[dynamic_diag$Metric == metric_name]
      if (length(val) == 0) return(NA)
      val[1]
    }
    new_results$cost_profile$dynamic_max_R0_multiplier_requested <- get_dynamic_diag_value("dynamic_max_R0_multiplier_requested")
    new_results$cost_profile$dynamic_R0_cap_requested <- get_dynamic_diag_value("dynamic_R0_cap_requested")
    new_results$cost_profile$dynamic_max_candidate_R0_observed <- get_dynamic_diag_value("dynamic_max_candidate_R0_observed")
    new_results$cost_profile$dynamic_max_candidate_advantage_observed <- get_dynamic_diag_value("dynamic_max_candidate_advantage_observed")
    new_results$cost_profile$dynamic_max_established_variant_R0 <- get_dynamic_diag_value("dynamic_max_established_variant_R0")
    new_results$cost_profile$dynamic_max_established_variant_advantage <- get_dynamic_diag_value("dynamic_max_established_variant_advantage")
    new_results$cost_profile$dynamic_mean_established_variant_R0 <- get_dynamic_diag_value("dynamic_mean_established_variant_R0")
    new_results$cost_profile$dynamic_dominant_variant_id_final <- get_dynamic_diag_value("dynamic_dominant_variant_id_final")
    new_results$cost_profile$dynamic_dominant_variant_share_final <- get_dynamic_diag_value("dynamic_dominant_variant_share_final")
    new_results$cost_profile$dynamic_dominant_variant_R0_final <- get_dynamic_diag_value("dynamic_dominant_variant_R0_final")
    new_results$cost_profile$dynamic_candidate_cap_binding_events <- get_dynamic_diag_value("dynamic_candidate_cap_binding_events")
    new_results$cost_profile$dynamic_established_cap_binding_events <- get_dynamic_diag_value("dynamic_established_cap_binding_events")
    new_results$cost_profile$dynamic_cap_usage_note <- get_dynamic_diag_value("dynamic_cap_usage_note")
    new_results$cost_profile$dynamic_monotonicity_note <- get_dynamic_diag_value("dynamic_monotonicity_note")
    new_results$cost_profile$dynamic_adaptive_saturation <- ifelse(isTRUE(dynamic_config$adaptive_saturation), "TRUE", "FALSE")
    new_results$cost_profile$dynamic_saturation_exponent <- dynamic_config$saturation_exponent
    if (!is.null(new_results$candidate_mutations) && nrow(new_results$candidate_mutations) > 0 && "adaptive_space_multiplier" %in% names(new_results$candidate_mutations)) {
      new_results$cost_profile$dynamic_mean_adaptive_space_multiplier <- sprintf("%.4f", mean(new_results$candidate_mutations$adaptive_space_multiplier, na.rm = TRUE))
      if ("expressed_fitness_effect" %in% names(new_results$candidate_mutations)) {
        new_results$cost_profile$dynamic_max_expressed_advantage_observed <- sprintf("%.4f", max(new_results$candidate_mutations$expressed_fitness_effect / 100, na.rm = TRUE))
      }
    } else {
      new_results$cost_profile$dynamic_mean_adaptive_space_multiplier <- NA
      new_results$cost_profile$dynamic_max_expressed_advantage_observed <- NA
    }

    print("=== DIFFUSION DIAGNOSTIC ===")
    print(selected_starting_country)
    print(sum(new_results$passenger_matrix_annual[selected_starting_country, ], na.rm = TRUE))
    print(sort(new_results$passenger_matrix_annual[selected_starting_country, ], decreasing = TRUE)[1:10])
    print("Countries with >=1 active infection at final day:")
    print(sum(tail(new_results$country_history, 1) >= 1, na.rm = TRUE))
    print("Cumulative imported seeds by final day:")
    print(sum(new_results$new_country_import_history, na.rm = TRUE))
    computation_status(sprintf(
      paste0(
        "Calculation completed.\n",
        "Model mode: %s\n",
        "Run-event elapsed time: %s seconds\n",
        "Days: %s\nCountries: %s\nFinal active infections: %s\nCumulative deaths: %s\nCountries reached: %s\nExpected imported seeds: %s\nEstablished variants: %s\nFinal effective R0: %.2f"      ),
      input$model_structure,
      sprintf("%.2f", run_event_elapsed),
      days,
      length(COUNTRIES_LIST),
      format(round(tail(new_results$data$I, 1)), big.mark = ","),
      format(round(tail(new_results$data$D, 1)), big.mark = ","),
      sum(!is.na(new_results$first_reached_day)),
      format(round(sum(new_results$expected_import_history, na.rm = TRUE), 2), big.mark = ","),
      new_results$total_variants + 1,
      new_results$final_R0
    ))
    covid_results <- slice_fixed_reference(selected_fixed_reference_cache(), days)


    sim_data$hantavirus <- list(
      data = new_results$data,
      metrics = calculate_metrics(new_results$data, N),
      R0 = input$R0,
      final_R0 = new_results$final_R0,
      variants_emerged = new_results$variants_emerged,
      candidate_mutations = new_results$candidate_mutations,
      variant_frequency_history = new_results$variant_frequency_history,
      R0_history = new_results$R0_history,
      country_history = new_results$country_history,
      exposed_country_history = new_results$exposed_country_history,
      death_country_history = new_results$death_country_history,
      new_country_import_history = new_results$new_country_import_history,
      expected_import_history = new_results$expected_import_history,
      infectious_pressure_history = new_results$infectious_pressure_history,
      infectivity_kernel = new_results$infectivity_kernel,
      remaining_infectivity = new_results$remaining_infectivity,
      first_reached_day = new_results$first_reached_day,
      passenger_matrix_annual = new_results$passenger_matrix_annual,
      total_variants = new_results$total_variants,
      age_parameters = new_results$age_parameters,
      age_history = new_results$age_history,
      age_summary = new_results$age_summary,
      model_structure = new_results$model_structure,
      cost_profile = new_results$cost_profile,
      containment_schedule = new_results$containment_schedule,
      color = "#E74C3C"
    )

    sim_data$covid <- list(
      data = covid_results$data,
      metrics = calculate_metrics(covid_results$data, N),
      R0 = covid_results$R0,
      final_R0 = covid_results$final_R0,
      variants_emerged = covid_results$variants_emerged,
      candidate_mutations = covid_results$candidate_mutations,
      variant_frequency_history = covid_results$variant_frequency_history,
      R0_history = covid_results$R0_history,
      country_history = covid_results$country_history,
      exposed_country_history = covid_results$exposed_country_history,
      death_country_history = covid_results$death_country_history,
      new_country_import_history = covid_results$new_country_import_history,
      expected_import_history = covid_results$expected_import_history,
      infectious_pressure_history = covid_results$infectious_pressure_history,
      infectivity_kernel = covid_results$infectivity_kernel,
      remaining_infectivity = covid_results$remaining_infectivity,
      first_reached_day = covid_results$first_reached_day,
      passenger_matrix_annual = covid_results$passenger_matrix_annual,
      total_variants = covid_results$total_variants,
      compartment_model = if (!is.null(covid_results$compartment_model)) covid_results$compartment_model else "SIRD",
      reference_type = covid_results$reference_type,
      reference_file = covid_results$reference_file,
      color = "#27AE60"
    )

    auto_play_after_run <- isTRUE(input$auto_play_after_run)
    if (auto_play_after_run) animation_running(TRUE)
    session$onFlushed(function() {
      updateSliderInput(session, "time_slider", value = 0)
      updateTabsetPanel(session, "main_tabs", selected = "World map")
      session$sendCustomMessage("scrollToMainMap", TRUE)
      if (!auto_play_after_run) animation_running(FALSE)
    }, once = TRUE)
  })

  global_plot_values <- function(x) {
    if (identical(input$global_plot_scale, "percent")) x / WORLD_POPULATION * 100 else x
  }
  global_axis_title <- function(metric_label) {
    if (identical(input$global_plot_scale, "percent")) paste0(metric_label, " (% of world population)") else paste0(metric_label, " (persons)")
  }
  global_hover <- function(virus_label, metric_label) {
    if (identical(input$global_plot_scale, "percent")) {
      paste0("<b>", virus_label, "</b><br>Day: %{x}<br>", metric_label, ": %{y:.4f}%<extra></extra>")
    } else {
      paste0("<b>", virus_label, "</b><br>Day: %{x}<br>", metric_label, ": %{y:,.0f} persons<extra></extra>")
    }
  }
  get_peak_info <- function(df) {
    idx <- which.max(df$I)
    list(day = df$time[idx], infected = df$I[idx])
  }
  add_infection_peak_lines <- function(p, h_peak, c_peak, y_values) {
    ymax <- max(y_values, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    h_label <- if (identical(input$global_plot_scale, "percent")) sprintf("New virus peak | day: %s | %.4f%%", h_peak$day, h_peak$infected / WORLD_POPULATION * 100) else sprintf("New virus peak | day: %s | persons: %s", h_peak$day, format_big(h_peak$infected))
    c_label <- if (identical(input$global_plot_scale, "percent")) sprintf("Omicron reference peak | day: %s | %.4f%%", c_peak$day, c_peak$infected / WORLD_POPULATION * 100) else sprintf("Omicron reference peak | day: %s | persons: %s", c_peak$day, format_big(c_peak$infected))
    p %>% layout(
      shapes = list(
        list(type = "line", x0 = h_peak$day, x1 = h_peak$day, y0 = 0, y1 = ymax, line = list(color = "#E74C3C", width = 1, dash = "dash")),
        list(type = "line", x0 = c_peak$day, x1 = c_peak$day, y0 = 0, y1 = ymax, line = list(color = "#27AE60", width = 1, dash = "dot"))
      ),
      annotations = list(
        list(x = h_peak$day, y = ymax, text = h_label, showarrow = TRUE, ax = 40, ay = -30, font = list(size = 11)),
        list(x = c_peak$day, y = ymax * 0.90, text = c_label, showarrow = TRUE, ax = 40, ay = -30, font = list(size = 11))
      )
    )
  }

  output$plot_infected <- renderPlotly({
    req(sim_data$hantavirus$data, sim_data$covid$data)
    h <- sim_data$hantavirus$data; c <- sim_data$covid$data
    y_h <- global_plot_values(h$I); y_c <- global_plot_values(c$I)
    h_peak <- get_peak_info(h); c_peak <- get_peak_info(c)
    p <- plot_ly() %>%
      add_trace(x = h$time, y = y_h, name = "New Virus", type = "scatter", mode = "lines", line = list(color = "#E74C3C", width = 3), hovertemplate = global_hover("New Virus", "Active infected")) %>%
      add_trace(x = c$time, y = y_c, name = "COVID-19 Omicron reference (origin: South Africa)", type = "scatter", mode = "lines", line = list(color = "#27AE60", width = 3), hovertemplate = global_hover("COVID-19", "Active infected")) %>%
      layout(title = "Active infected population over time", xaxis = list(title = "Days"), yaxis = list(title = global_axis_title("Active infected")), legend = list(orientation = "h", x = 0, y = 1.12))
    add_infection_peak_lines(p, h_peak, c_peak, c(y_h, y_c)) %>% config(displayModeBar = TRUE)
  })

  output$plot_deaths <- renderPlotly({
    req(sim_data$hantavirus$data, sim_data$covid$data)
    h <- sim_data$hantavirus$data; c <- sim_data$covid$data
    y_h <- global_plot_values(h$D); y_c <- global_plot_values(c$D)
    h_peak <- get_peak_info(h); c_peak <- get_peak_info(c)
    p <- plot_ly() %>%
      add_trace(x = h$time, y = y_h, name = "New Virus", type = "scatter", mode = "lines", line = list(color = "#E74C3C", width = 3), hovertemplate = global_hover("New Virus", "Cumulative deaths"), showlegend = FALSE) %>%
      add_trace(x = c$time, y = y_c, name = "COVID-19 Omicron reference (origin: South Africa)", type = "scatter", mode = "lines", line = list(color = "#27AE60", width = 3), hovertemplate = global_hover("COVID-19", "Cumulative deaths"), showlegend = FALSE) %>%
      layout(title = "Cumulative deaths over time", xaxis = list(title = "Days"), yaxis = list(title = global_axis_title("Cumulative deaths")), showlegend = FALSE)
    p %>% config(displayModeBar = TRUE)
  })

  output$plot_recovered <- renderPlotly({
    req(sim_data$hantavirus$data, sim_data$covid$data)
    h <- sim_data$hantavirus$data; c <- sim_data$covid$data
    y_h <- global_plot_values(h$R); y_c <- global_plot_values(c$R)
    h_peak <- get_peak_info(h); c_peak <- get_peak_info(c)
    p <- plot_ly() %>%
      add_trace(x = h$time, y = y_h, name = "New Virus", type = "scatter", mode = "lines", line = list(color = "#E74C3C", width = 3), hovertemplate = global_hover("New Virus", "Cumulative recovered"), showlegend = FALSE) %>%
      add_trace(x = c$time, y = y_c, name = "COVID-19 Omicron reference (origin: South Africa)", type = "scatter", mode = "lines", line = list(color = "#27AE60", width = 3), hovertemplate = global_hover("COVID-19", "Cumulative recovered"), showlegend = FALSE) %>%
      layout(title = "Cumulative recovered over time", xaxis = list(title = "Days"), yaxis = list(title = global_axis_title("Cumulative recovered")), showlegend = FALSE)
    p %>% config(displayModeBar = TRUE)
  })

  output$global_peak_summary_table <- renderTable({
    req(sim_data$hantavirus$data, sim_data$covid$data)
    h_peak <- get_peak_info(sim_data$hantavirus$data)
    c_peak <- get_peak_info(sim_data$covid$data)
    data.frame(
      Series = c("New virus", "Omicron reference"),
      `Peak day` = c(h_peak$day, c_peak$day),
      `Peak active infected` = c(format_big(h_peak$infected), format_big(c_peak$infected)),
      `Peak active infected (% world)` = c(round(h_peak$infected / WORLD_POPULATION * 100, 4), round(c_peak$infected / WORLD_POPULATION * 100, 4)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, rownames = FALSE)

  output$connectivity_preview_map <- renderLeaflet({
    origin <- input$starting_country
    if (is.null(origin) || !origin %in% COUNTRIES_LIST) origin <- COUNTRIES_LIST[1]
    top_n <- 15L
    scenario <- if (!is.null(input$air_travel_scenario)) input$air_travel_scenario else "reference"

    matrix <- make_bilateral_passenger_matrix(COUNTRIES_LIST, air_travel_scenario = scenario, rng_seed = NULL, route_noise_sdlog = 0)
    flows <- matrix[origin, ]
    flows <- flows[names(flows) != origin]
    flows <- flows[is.finite(flows) & flows > 0]
    flows <- sort(flows, decreasing = TRUE)
    flows <- head(flows, top_n)

    coords <- COUNTRY_COORDS
    origin_coord <- coords[coords$country == origin, , drop = FALSE]

    m <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = origin_coord$lng[1], lat = origin_coord$lat[1], zoom = 3)

    if (length(flows) == 0 || nrow(origin_coord) == 0) {
      return(m %>% addControl(html = HTML("<div style='background:white; padding:8px;'>No connectivity preview available.</div>"), position = "bottomright"))
    }

    max_flow <- max(flows, na.rm = TRUE)
    route_rows <- data.frame(dest = names(flows), annual_passengers = as.numeric(flows), stringsAsFactors = FALSE) %>%
      left_join(coords, by = c("dest" = "country")) %>%
      filter(!is.na(lng), !is.na(lat))

    m <- m %>% addCircleMarkers(
      lng = origin_coord$lng[1], lat = origin_coord$lat[1], radius = 8,
      color = "#111111", fillColor = "#E74C3C", fillOpacity = 0.95, weight = 3,
      popup = paste0("<b>Initial country</b><br>", origin)
    )

    if (nrow(route_rows) > 0) {
      for (i in seq_len(nrow(route_rows))) {
        w <- 1.5 + 6 * sqrt(route_rows$annual_passengers[i] / max_flow)
        m <- m %>% addPolylines(
          lng = c(origin_coord$lng[1], route_rows$lng[i]),
          lat = c(origin_coord$lat[1], route_rows$lat[i]),
          color = "#2E86C1",
          weight = w,
          opacity = 0.55,
          popup = paste0(
            "<b>", origin, " → ", route_rows$dest[i], "</b><br>",
            "Annual passenger-weight used by model: ", format_big(route_rows$annual_passengers[i], 0)
          )
        )
      }
      m <- m %>% addCircleMarkers(
        data = route_rows,
        lng = ~lng, lat = ~lat, radius = 5,
        color = "#2E86C1", fillColor = "#AED6F1", fillOpacity = 0.85, weight = 1,
        popup = ~paste0("<b>", dest, "</b><br>Connection weight: ", format_big(annual_passengers, 0))
      )
    }

    legend_html <- HTML(paste0(
      "<div style='background:white; padding:8px 10px; border-radius:4px; box-shadow:0 1px 5px rgba(0,0,0,0.35); font-size:12px;'>",
      "<b>Connectivity preview</b><br>",
      "Origin: ", origin, "<br>",
      "Top connections shown: ", nrow(route_rows), "<br>",
      "Line width = relative passenger weight",
      "</div>"
    ))
    m %>% addControl(html = legend_html, position = "bottomright")
  })

  output$world_map_infected <- renderLeaflet({ leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = 0, lat = 20, zoom = 2) })
  output$world_map_deaths <- renderLeaflet({ leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = 0, lat = 20, zoom = 2) })

  make_map_legend <- function(title, selected_day, panel_max_value, current_day_max_value, color_low, color_high, empty_message = NULL, boost_visibility = TRUE, choropleth = FALSE, polygon_note = NULL) {
    scale_note <- if (isTRUE(choropleth) && isTRUE(boost_visibility)) {
      "Country fill color uses pseudo-log relative burden per 100,000 population. Countries with <1 absolute event remain unfilled."
    } else if (isTRUE(choropleth)) {
      "Country fill color uses relative burden per 100,000 population. Countries with <1 absolute event remain unfilled."
    } else if (isTRUE(boost_visibility)) {
      "Marker size uses pseudo-log scaling with a visible minimum radius for non-zero values."
    } else {
      "Marker area approximately proportional to value."
    }
    body_text <- paste0(
      "<div style='margin-top:4px;'>Day: <b>", selected_day, "</b></div>",
      "<div>Max on selected day: <b>", format_big(current_day_max_value, 2), "</b></div>",
      "<div>Max in this panel over simulation: <b>", format_big(panel_max_value, 2), "</b></div>",
      "<div style='margin-top:6px; height:10px; background:linear-gradient(to right, ", color_low, ", ", color_high, "); border:1px solid #999;'></div>",
      "<div style='font-size:11px;'>", scale_note, "</div>",
      "<div style='font-size:11px;'>Black border = starting country. Purple border = active containment scope.</div>"
    )
    if (!is.null(polygon_note) && !is.na(polygon_note) && nzchar(polygon_note)) body_text <- paste0(body_text, "<div style='font-size:11px; margin-top:4px;'>", polygon_note, "</div>")
    if (!is.null(empty_message)) body_text <- paste0(body_text, "<div style='margin-top:4px;'>", empty_message, "</div>")
    HTML(paste0("<div style='background:white; padding:8px 10px; border-radius:4px; box-shadow:0 1px 5px rgba(0,0,0,0.35); font-size:12px; line-height:1.25;'><div style='font-weight:bold;'>", title, "</div>", body_text, "</div>"))
  }

  calculate_map_radius <- function(values, full_max, boost_visibility = TRUE, min_radius = 5, max_radius = 46) {
    values <- as.numeric(values)
    out <- rep(0, length(values))
    positive <- is.finite(values) & values > 0
    if (!any(positive)) return(out)
    if (!is.finite(full_max) || full_max <= 0) full_max <- max(values[positive], na.rm = TRUE)
    if (!is.finite(full_max) || full_max <= 0) full_max <- 1
    if (isTRUE(boost_visibility)) {
      denom <- log10(full_max + 1)
      if (!is.finite(denom) || denom <= 0) denom <- 1
      out[positive] <- min_radius + (max_radius - min_radius) * (log10(values[positive] + 1) / denom)
    } else {
      out[positive] <- pmax(2, pmin(max_radius, sqrt(values[positive] / full_max) * max_radius))
    }
    pmax(ifelse(positive, min_radius, 0), pmin(max_radius, out))
  }

  current_containment_countries_for_day <- function(selected_day) {
    schedule <- tryCatch(
      make_containment_schedule(
        enabled = ((!is.null(input$containment_preset) && !identical(input$containment_preset, "none"))),
        preset = input$containment_preset,
        start_day = input$containment_start_day,
        end_day = input$containment_end_day,
        custom_transmission_reduction = input$containment_transmission_reduction,
        custom_mobility_reduction = input$containment_mobility_reduction,
        label = ifelse(is.null(input$containment_label), "Containment period", input$containment_label),
        geographic_scope = input$containment_geographic_scope,
        affected_continents = input$containment_affected_continents,
        affected_countries = input$containment_affected_countries
      ),
      error = function(e) NULL
    )
    if (is.null(schedule) || !isTRUE(schedule$enabled)) return(character(0))
    if (!is.finite(schedule$start_day) || !is.finite(schedule$end_day)) return(character(0))
    if (selected_day >= schedule$start_day && selected_day <= schedule$end_day) schedule$affected_countries else character(0)
  }

  update_country_map <- function(map_id, history, selected_day, value_label, palette_colors, popup_label, empty_message, boost_visibility = TRUE, use_choropleth = TRUE) {
    values <- history[selected_day + 1, ]
    full_max <- max(history, na.rm = TRUE); if (!is.finite(full_max) || full_max <= 0) full_max <- 1
    current_max <- max(values, na.rm = TRUE); if (!is.finite(current_max) || current_max < 0) current_max <- 0
    starting_country <- if (!is.null(sim_data$hantavirus$starting_country_used)) sim_data$hantavirus$starting_country_used else isolate(input$starting_country)
    containment_countries <- current_containment_countries_for_day(selected_day)
    map_interactive <- !isTRUE(animation_running())
    # Redrawing 50 country polygons on every animation frame causes visible flickering
    # in Leaflet. During playback we use lightweight centroid markers; when the
    # map is paused or the day is changed manually, the country-border choropleth
    # is rendered again.
    effective_choropleth <- isTRUE(use_choropleth) && isTRUE(map_interactive)

    proxy <- leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearControls()

    polygons_available <- isTRUE(effective_choropleth) && world_polygon_available()
    polygon_note <- if (polygons_available) {
      paste0("Polygon source: ", basename(WORLD_COUNTRY_POLYGONS$file))
    } else if (isTRUE(use_choropleth) && !isTRUE(map_interactive)) {
      "Animation mode: marker-only display to avoid polygon flickering. Pause to inspect country borders and labels."
    } else if (isTRUE(use_choropleth)) {
      WORLD_COUNTRY_POLYGONS$warning
    } else {
      "Marker-only display selected."
    }

    if (polygons_available && requireNamespace("sf", quietly = TRUE)) {
      country_values <- data.frame(name = names(values), Value = as.numeric(values), stringsAsFactors = FALSE) %>%
        left_join(COUNTRY_TRAVEL_PARAMS[, c("country", "population_millions")], by = c("name" = "country")) %>%
        mutate(
          population = pmax(population_millions * 1e6, 1),
          RelativeValue = Value / population * 100000
        )
      full_rate_matrix <- sweep(history, 2, COUNTRY_TRAVEL_PARAMS$population_millions[match(colnames(history), COUNTRY_TRAVEL_PARAMS$country)] * 1e6, "/") * 100000
      full_rate_max <- max(full_rate_matrix, na.rm = TRUE)
      if (!is.finite(full_rate_max) || full_rate_max <= 0) full_rate_max <- 1
      current_rate_max <- max(country_values$RelativeValue, na.rm = TRUE)
      if (!is.finite(current_rate_max) || current_rate_max < 0) current_rate_max <- 0

      poly_data <- WORLD_COUNTRY_POLYGONS$data %>%
        dplyr::left_join(country_values, by = c("sim_country" = "name")) %>%
        dplyr::mutate(
          Value = ifelse(is.na(Value), 0, Value),
          RelativeValue = ifelse(is.na(RelativeValue), 0, RelativeValue),
          HasVisibleBurden = Value >= 1,
          ColorValue = ifelse(HasVisibleBurden, ifelse(isTRUE(boost_visibility), log10(pmax(RelativeValue, 0) + 1), RelativeValue), 0),
          IsStartingCountry = sim_country == starting_country,
          IsContainmentCountry = sim_country %in% containment_countries,
          StrokeColor = dplyr::case_when(
            IsStartingCountry ~ "#111111",
            IsContainmentCountry ~ "#7D3C98",
            TRUE ~ "#7F8C8D"
          ),
          StrokeWeight = dplyr::case_when(
            IsStartingCountry ~ 3,
            IsContainmentCountry ~ 2,
            TRUE ~ 0.7
          )
        )

      # Russia crosses the antimeridian. Some Leaflet renderers draw a spurious
      # horizontal band for that polygon in simplified global layers. To keep
      # the main map stable, Russia is shown as a centroid marker while the
      # remaining countries are drawn as polygons.
      russia_marker_data <- country_values %>%
        dplyr::filter(name == "Russia") %>%
        dplyr::left_join(COUNTRY_COORDS, by = c("name" = "country")) %>%
        dplyr::mutate(
          RelativeValue = ifelse(is.na(RelativeValue), 0, RelativeValue),
          Value = ifelse(is.na(Value), 0, Value),
          HasVisibleBurden = Value >= 1,
          ColorValue = ifelse(HasVisibleBurden, ifelse(isTRUE(boost_visibility), log10(pmax(RelativeValue, 0) + 1), RelativeValue), 0),
          IsStartingCountry = name == starting_country,
          IsContainmentCountry = name %in% containment_countries,
          StrokeColor = dplyr::case_when(
            IsStartingCountry ~ "#111111",
            IsContainmentCountry ~ "#7D3C98",
            TRUE ~ "#4A4A4A"
          ),
          StrokeWeight = dplyr::case_when(
            IsStartingCountry ~ 3,
            IsContainmentCountry ~ 2,
            TRUE ~ 1
          )
        )
      poly_data <- poly_data %>% dplyr::filter(sim_country != "Russia")

      color_domain_max <- if (isTRUE(boost_visibility)) log10(full_rate_max + 1) else full_rate_max
      if (!is.finite(color_domain_max) || color_domain_max <= 0) color_domain_max <- 1
      pal <- colorNumeric(palette = palette_colors, domain = c(0, color_domain_max), na.color = "#F7F9F9")
      legend_html <- make_map_legend(value_label, selected_day, full_rate_max, current_rate_max, palette_colors[1], palette_colors[length(palette_colors)], if (current_max < 1) empty_message else NULL, boost_visibility = boost_visibility, choropleth = TRUE, polygon_note = polygon_note)
      if (isTRUE(map_interactive)) {
        proxy <- proxy %>% addControl(html = legend_html, position = "bottomright")
      }
      polygon_popup <- if (isTRUE(map_interactive)) {
        ~paste0(
          ifelse(IsStartingCountry, "<b>Starting country</b><br>", ""),
          ifelse(IsContainmentCountry, "<b>Containment active here</b><br>", ""),
          "<b>", sim_country, "</b><br>Day: ", selected_day,
          "<br>", popup_label, ": ", format_big(Value, 2),
          "<br>Per 100,000 population: ", sprintf("%.2f", RelativeValue)
        )
      } else NULL
      proxy <- proxy %>% addPolygons(
        data = poly_data,
        fillColor = ~ifelse(HasVisibleBurden, pal(ColorValue), "#FFFFFF"),
        fillOpacity = ~ifelse(HasVisibleBurden, 0.82, 0.0),
        color = ~StrokeColor,
        weight = ~StrokeWeight,
        opacity = 0.95,
        smoothFactor = 0.2,
        popup = polygon_popup,
        options = leaflet::pathOptions(interactive = map_interactive)
      )
      if (nrow(russia_marker_data) > 0 && is.finite(russia_marker_data$lng[1]) && is.finite(russia_marker_data$lat[1])) {
        russia_popup <- if (isTRUE(map_interactive)) {
          paste0(
            ifelse(russia_marker_data$IsStartingCountry[1], "<b>Starting country</b><br>", ""),
            ifelse(russia_marker_data$IsContainmentCountry[1], "<b>Containment active here</b><br>", ""),
            "<b>Russia</b><br>Day: ", selected_day,
            "<br>", popup_label, ": ", format_big(russia_marker_data$Value[1], 2),
            "<br>Per 100,000 population: ", sprintf("%.2f", russia_marker_data$RelativeValue[1]),
            "<br><span style='font-size:11px;color:#566573;'>Shown as centroid marker to avoid antimeridian polygon artifacts.</span>"
          )
        } else NULL
        proxy <- proxy %>% addCircleMarkers(
          data = russia_marker_data,
          lng = ~lng,
          lat = ~lat,
          radius = ~ifelse(HasVisibleBurden, 8, 4),
          color = ~StrokeColor,
          fillColor = ~ifelse(HasVisibleBurden, pal(ColorValue), "#FFFFFF"),
          fillOpacity = ~ifelse(HasVisibleBurden, 0.90, 0.0),
          stroke = TRUE,
          weight = ~StrokeWeight,
          popup = russia_popup,
          options = leaflet::pathOptions(interactive = map_interactive)
        )
      }
      return(proxy)
    }

    marker_data <- data.frame(name = names(values), Value = as.numeric(values), stringsAsFactors = FALSE) %>%
      left_join(COUNTRY_COORDS, by = c("name" = "country")) %>%
      filter(!is.na(lng), !is.na(lat), Value >= 1) %>%
      mutate(
        MapRadius = calculate_map_radius(Value, full_max, boost_visibility = boost_visibility),
        IsStartingCountry = .data$name == starting_country,
        IsContainmentCountry = .data$name %in% containment_countries,
        StrokeColor = dplyr::case_when(
          IsStartingCountry ~ "#111111",
          IsContainmentCountry ~ "#7D3C98",
          TRUE ~ "#4A4A4A"
        ),
        StrokeWeight = dplyr::case_when(
          IsStartingCountry ~ 3,
          IsContainmentCountry ~ 2,
          TRUE ~ 1
        ),
        PopupPrefix = paste0(ifelse(IsStartingCountry, "<b>Starting country</b><br>", ""), ifelse(IsContainmentCountry, "<b>Containment active here</b><br>", ""))
      )
    pal <- colorNumeric(palette = palette_colors, domain = c(0, full_max))
    legend_html <- make_map_legend(value_label, selected_day, full_max, current_max, palette_colors[1], palette_colors[length(palette_colors)], if (current_max < 1 || nrow(marker_data) == 0) empty_message else NULL, boost_visibility = boost_visibility, choropleth = FALSE, polygon_note = polygon_note)
    if (isTRUE(map_interactive)) {
      proxy <- proxy %>% addControl(html = legend_html, position = "bottomright")
    }
    if (nrow(marker_data) > 0) {
      marker_popup <- if (isTRUE(map_interactive)) {
        ~paste0(PopupPrefix, "<b>", name, "</b><br>Day: ", selected_day, "<br>", popup_label, ": ", format_big(Value, 2))
      } else NULL
      proxy %>% addCircleMarkers(
        data = marker_data,
        lng = ~lng,
        lat = ~lat,
        radius = ~MapRadius,
        color = ~StrokeColor,
        fillColor = ~pal(Value),
        fillOpacity = 0.88,
        popup = marker_popup,
        stroke = TRUE,
        weight = ~StrokeWeight,
        options = leaflet::pathOptions(interactive = map_interactive)
      )
    }
  }

  refresh_current_maps <- function() {
    # Must be called from an observer/reactive context. It intentionally reads
    # input$time_slider and animation_running() reactively so the map updates
    # while the slider advances during playback and switches back to choropleth
    # immediately when playback is paused.
    req(sim_data$hantavirus$country_history, sim_data$hantavirus$death_country_history)
    selected_day <- min(max(input$time_slider, 0), nrow(sim_data$hantavirus$country_history) - 1)
    boost_visibility <- TRUE
    use_choropleth <- TRUE
    update_country_map("world_map_infected", sim_data$hantavirus$country_history, selected_day, "Active cases per 100,000", c("#FFF3B0", "#F39C12", "#C0392B", "#641E16"), "Active cases", paste0("No active cases displayed at day ", selected_day), boost_visibility = boost_visibility, use_choropleth = use_choropleth)
    update_country_map("world_map_deaths", sim_data$hantavirus$death_country_history, selected_day, "Cumulative deaths per 100,000", c("#D6EAF8", "#3498DB", "#21618C", "#0B1F33"), "Cumulative deaths", paste0("No deaths at day ", selected_day), boost_visibility = boost_visibility, use_choropleth = use_choropleth)
  }

  observe({
    refresh_current_maps()
  })

  observeEvent(input$import_scenario_config, {
    req(input$import_scenario_config_json$datapath)
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      showNotification("No se puede importar: falta el paquete jsonlite.", type = "error", duration = 6)
      return(NULL)
    }
    cfg <- tryCatch(jsonlite::read_json(input$import_scenario_config_json$datapath, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(cfg)) {
      showNotification("Could not read the configuration JSON.", type = "error", duration = 6)
      return(NULL)
    }
    apply_scenario_config(cfg)
    showNotification("Configuration imported. Click Run simulation to recalculate.", type = "message", duration = 5)
  })

  observeEvent(input$save_current_scenario, {
    req(sim_data$hantavirus$data)
    scenario_library$counter <- scenario_library$counter + 1
    id <- paste0("scenario_", scenario_library$counter)
    nm <- input$scenario_name
    if (is.null(nm) || !nzchar(trimws(nm))) nm <- paste("Scenario", scenario_library$counter)
    cfg <- capture_current_scenario_config()
    summary <- scenario_summary_from_results(nm, cfg, sim_data$hantavirus)
    scenario_library$items[[id]] <- list(
      id = id,
      name = nm,
      config = cfg,
      summary = summary,
      data = sim_data$hantavirus$data,
      R0_history = sim_data$hantavirus$R0_history,
      saved_at = as.character(Sys.time())
    )
    update_scenario_choices()
    showNotification(paste("Saved scenario:", nm), type = "message", duration = 3)
  })

  observeEvent(input$duplicate_selected_scenario, {
    id <- input$selected_saved_scenario
    req(id, scenario_library$items[[id]])
    scenario_library$counter <- scenario_library$counter + 1
    new_id <- paste0("scenario_", scenario_library$counter)
    item <- scenario_library$items[[id]]
    item$id <- new_id
    item$name <- paste0(item$name, " copia")
    if (!is.null(item$summary)) item$summary$Name <- item$name
    item$saved_at <- as.character(Sys.time())
    scenario_library$items[[new_id]] <- item
    update_scenario_choices()
  })

  observeEvent(input$delete_selected_scenario, {
    id <- input$selected_saved_scenario
    req(id)
    scenario_library$items[[id]] <- NULL
    update_scenario_choices()
  })

  observeEvent(input$restore_selected_scenario, {
    id <- input$selected_saved_scenario
    req(id, scenario_library$items[[id]])
    apply_scenario_config(scenario_library$items[[id]]$config)
    showNotification("Configuration restored. Click Run simulation to recalculate.", type = "message", duration = 4)
  })

  observeEvent(input$compare_saved_scenario, {
    id <- input$selected_saved_scenario
    req(id, scenario_library$items[[id]])
    scenario_library$compare_id <- id
  })

  selected_scenario_item <- reactive({
    id <- input$selected_saved_scenario
    if (is.null(id) || !nzchar(id)) return(NULL)
    scenario_library$items[[id]]
  })

  output$selected_scenario_card <- renderUI({
    item <- selected_scenario_item()
    if (is.null(item) || is.null(item$summary)) {
      return(tags$div(class = "scenario-card-polished", tags$p("No scenario selected.")))
    }
    s <- item$summary
    comparator <- NULL
    if (!is.null(sim_data$covid$data)) {
      covid_like <- list(
        data = sim_data$covid$data,
        first_reached_day = sim_data$covid$first_reached_day,
        compartment_model = if (!is.null(sim_data$covid$compartment_model)) sim_data$covid$compartment_model else "SIRD",
        exposed_period_days = if (!is.null(sim_data$covid$exposed_period_days)) sim_data$covid$exposed_period_days else NA,
        cost_profile = list(rds_reference = if (!is.null(sim_data$covid$reference_file)) sim_data$covid$reference_file else "COVID comparator", comparator_structure_warning = "reference")
      )
      cfg <- list(simulation_days = item$config$simulation_days, R0 = 4.25, exposed_period_days = NA, infectious_period_days = NA, mortality_rate = NA, starting_country = "South Africa", disease_compartment_model = covid_like$compartment_model)
      comparator <- scenario_summary_from_results("COVID comparator", cfg, covid_like)
    }
    tags$div(class = "scenario-card-polished",
      h3(s$Name),
      div(class = "scenario-subtitle",
        tags$span(class = "scenario-tag", if (!is.null(s$Label)) s$Label else "Scenario"),
        tags$span(class = "scenario-tag", s$Model),
        tags$span(class = "scenario-tag", paste("Start:", s$StartCountry))
      ),
      tags$div(class = "scenario-kpi-grid",
        tags$div(class = "scenario-kpi", tags$div(class = "scenario-kpi-label", "Peak day"), tags$div(class = "scenario-kpi-value", s$PeakDay)),
        tags$div(class = "scenario-kpi", tags$div(class = "scenario-kpi-label", "Peak active"), tags$div(class = "scenario-kpi-value", format(round(s$PeakActive), big.mark = ","))),
        tags$div(class = "scenario-kpi", tags$div(class = "scenario-kpi-label", "Peak exposed"), tags$div(class = "scenario-kpi-value", ifelse(is.na(s$PeakExposed), "NA", format(round(s$PeakExposed), big.mark = ",")))),
        tags$div(class = "scenario-kpi", tags$div(class = "scenario-kpi-label", "Final deaths"), tags$div(class = "scenario-kpi-value", format(round(s$FinalDeaths), big.mark = ",")))
      ),
      tags$p(tags$b("Countries reached: "), s$CountriesReached),
      tags$p(tags$b("Comparator: "), s$Comparator),
      tags$p(tags$b("Structure warning: "), s$StructureWarning),
      tags$div(class = "scenario-interpretation", build_scenario_interpretation(s, comparator))
    )
  })

  scenario_summary_df <- reactive({
    items <- scenario_library$items
    if (length(items) == 0) return(data.frame())
    dplyr::bind_rows(lapply(items, function(x) x$summary))
  })

  output$scenario_library_table <- renderTable({
    df <- scenario_summary_df()
    if (nrow(df) == 0) return(data.frame(Message = "No saved scenarios."))
    df[, c("Name", "Label", "Model", "StartCountry", "Days", "R0", "PeakDay", "PeakActive", "FinalDeaths", "CountriesReached", "Comparator", "StructureWarning"), drop = FALSE]
  }, digits = 2)

  output$scenario_comparison_table <- renderTable({
    out <- current_comparison_rows()
    if (nrow(out) == 0) return(data.frame(Message = "Run or save a scenario to compare."))
    keep <- intersect(c("Name", "Label", "Model", "StartCountry", "PeakDay", "PeakActive", "PeakActivePercentWorld", "PeakExposedDay", "PeakExposed", "FinalDeaths", "CountriesReached", "DeltaPeakDayVsComparator", "DeltaPeakActiveVsComparator", "DeltaPeakActivePercentVsComparator", "DeltaFinalDeathsVsComparator", "DeltaFinalDeathsPercentVsComparator"), names(out))
    out[, keep, drop = FALSE]
  }, digits = 2)

  output$scenario_comparison_plot <- renderPlotly({
    traces <- list()
    p <- plot_ly()
    if (!is.null(sim_data$hantavirus$data)) {
      p <- p %>% add_lines(data = sim_data$hantavirus$data, x = ~time, y = ~I, name = "Current simulation", line = list(width = 3))
    }
    compare_id <- scenario_library$compare_id
    if (!is.null(compare_id) && !is.null(scenario_library$items[[compare_id]])) {
      item <- scenario_library$items[[compare_id]]
      p <- p %>% add_lines(data = item$data, x = ~time, y = ~I, name = item$name, line = list(width = 2, dash = "dash"))
    }
    if (!is.null(sim_data$covid$data)) {
      p <- p %>% add_lines(data = sim_data$covid$data, x = ~time, y = ~I, name = "Comparator", line = list(width = 2, dash = "dot"))
    }
    p %>% layout(
      xaxis = list(title = "Day"),
      yaxis = list(title = "Active population"),
      legend = list(orientation = "h", x = 0, y = 1.08)
    )
  })

  output$download_scenario_summary_csv <- downloadHandler(
    filename = function() paste0("scenario_library_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- scenario_summary_df()
      if (nrow(df) == 0) df <- data.frame(Message = "No saved scenarios")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_scenario_config_json <- downloadHandler(
    filename = function() {
      item <- selected_scenario_item()
      nm <- if (!is.null(item)) gsub("[^A-Za-z0-9_]+", "_", item$name) else "scenario"
      paste0(nm, "_config.json")
    },
    content = function(file) {
      item <- selected_scenario_item()
      req(item)
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::write_json(item$config, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      } else {
        writeLines(c("{", paste0('  "error": "jsonlite package not available"'), "}"), file)
      }
    }
  )

  output$download_scenario_card_html <- downloadHandler(
    filename = function() {
      item <- selected_scenario_item()
      nm <- if (!is.null(item)) gsub("[^A-Za-z0-9_]+", "_", item$name) else "scenario"
      paste0(nm, "_card.html")
    },
    content = function(file) {
      item <- selected_scenario_item()
      req(item, item$summary)
      s <- item$summary
      html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'><title>", htmltools::htmlEscape(s$Name), "</title>",
        "<style>body{font-family:Arial,sans-serif;margin:24px;} .card{border:1px solid #ccc;border-radius:10px;padding:18px;max-width:780px;} h1{margin-top:0;} table{border-collapse:collapse;width:100%;} td{border-bottom:1px solid #eee;padding:8px;} td:first-child{font-weight:bold;width:40%;}</style>",
        "</head><body><div class='card'><h1>", htmltools::htmlEscape(s$Name), "</h1><table>",
        "<tr><td>Modelo</td><td>", s$Model, "</td></tr>",
        "<tr><td>Initial country</td><td>", s$StartCountry, "</td></tr>",
        "<tr><td>R0</td><td>", s$R0, "</td></tr>",
        "<tr><td>Active peak day</td><td>", s$PeakDay, "</td></tr>",
        "<tr><td>Active peak</td><td>", format(round(s$PeakActive), big.mark = ","), "</td></tr>",
        "<tr><td>Final deaths</td><td>", format(round(s$FinalDeaths), big.mark = ","), "</td></tr>",
        "<tr><td>Countries reached</td><td>", s$CountriesReached, "</td></tr>",
        "<tr><td>Comparator</td><td>", s$Comparator, "</td></tr>",
        "<tr><td>Structural warning</td><td>", s$StructureWarning, "</td></tr>",
        "</table><p>Card generated from the scenario lab.</p></div></body></html>"
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )

  output$download_scenario_comparison_csv <- downloadHandler(
    filename = function() paste0("scenario_comparison_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- current_comparison_rows()
      if (nrow(df) == 0) df <- data.frame(Message = "No comparison available")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_scenario_comparison_html <- downloadHandler(
    filename = function() paste0("scenario_comparison_", Sys.Date(), ".html"),
    content = function(file) {
      df <- current_comparison_rows()
      if (nrow(df) == 0) df <- data.frame(Message = "No comparison available")
      keep <- intersect(c("Name", "Model", "StartCountry", "PeakDay", "PeakActive", "FinalDeaths", "CountriesReached", "DeltaPeakDayVsComparator", "DeltaPeakActiveVsComparator", "DeltaFinalDeathsVsComparator"), names(df))
      df2 <- df[, keep, drop = FALSE]
      rows <- apply(df2, 1, function(x) paste0("<tr>", paste0("<td>", htmltools::htmlEscape(as.character(x)), "</td>", collapse = ""), "</tr>"))
      headers <- paste0("<tr>", paste0("<th>", htmltools::htmlEscape(names(df2)), "</th>", collapse = ""), "</tr>")
      html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'><title>Scenario comparison</title>",
        "<style>body{font-family:Arial,sans-serif;margin:24px;} table{border-collapse:collapse;width:100%;} th,td{border:1px solid #ddd;padding:8px;} th{background:#f5f5f5;} h1{margin-top:0;}</style>",
        "</head><body><h1>Scenario comparison</h1><table>", headers, paste(rows, collapse = ""), "</table></body></html>"
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )

  output$country_distribution <- renderPlotly({
    req(sim_data$hantavirus$country_history)
    history <- sim_data$hantavirus$country_history
    selected_day <- min(max(input$time_slider, 0), nrow(history) - 1)
    infected <- history[selected_day + 1, ]
    df <- data.frame(Country = names(infected), Infected = as.numeric(infected), stringsAsFactors = FALSE) %>% filter(Infected >= 0.01) %>% arrange(desc(Infected))
    if (nrow(df) == 0) df <- data.frame(Country = "No active countries", Infected = 0)
    plot_ly(df, x = ~reorder(Country, -Infected), y = ~Infected, type = "bar", marker = list(color = "#E74C3C"), hovertemplate = "<b>%{x}</b><br>Active infected: %{y:,.2f}<extra></extra>") %>%
      layout(title = paste("Infection Distribution by Country - Day", selected_day), xaxis = list(title = "Country", tickangle = -45), yaxis = list(title = "Active infected")) %>% config(displayModeBar = TRUE)
  })

  output$country_spread_timeline <- renderPlotly({
    req(sim_data$hantavirus$country_history)
    history <- sim_data$hantavirus$country_history
    df <- data.frame(
      Day = seq(0, nrow(history) - 1),
      Countries_With_Any_Expected_Infection = rowSums(history > 0, na.rm = TRUE),
      Countries_With_At_Least_0_01_Active = rowSums(history >= 0.01, na.rm = TRUE),
      Countries_With_Active_Transmission = rowSums(history >= 1, na.rm = TRUE),
      Countries_With_At_Least_1000_Active = rowSums(history >= 1000, na.rm = TRUE)
    )
    plot_ly(df) %>%
      add_trace(x = ~Day, y = ~Countries_With_Any_Expected_Infection, name = ">0 expected active infection", type = "scatter", mode = "lines", line = list(color = "#F5B7B1", width = 2)) %>%
      add_trace(x = ~Day, y = ~Countries_With_At_Least_0_01_Active, name = "≥0.01 expected active infections", type = "scatter", mode = "lines", line = list(color = "#F1948A", width = 2)) %>%
      add_trace(x = ~Day, y = ~Countries_With_Active_Transmission, name = "≥1 active case", type = "scatter", mode = "lines", line = list(color = "#E74C3C", width = 3)) %>%
      add_trace(x = ~Day, y = ~Countries_With_At_Least_1000_Active, name = "≥1,000 active cases", type = "scatter", mode = "lines", line = list(color = "#922B21", width = 2)) %>%
      layout(title = "Countries Reached Over Time", xaxis = list(title = "Day"), yaxis = list(title = "Number of countries")) %>% config(displayModeBar = TRUE)
  })

  output$top_countries_table <- renderTable({
    req(sim_data$hantavirus$country_history, sim_data$hantavirus$new_country_import_history)
    history <- sim_data$hantavirus$country_history
    imports <- sim_data$hantavirus$new_country_import_history
    selected_day <- min(max(input$time_slider, 0), nrow(history) - 1)
    infected <- history[selected_day + 1, ]
    cumulative_imports <- colSums(imports[seq_len(selected_day + 1), , drop = FALSE], na.rm = TRUE)
    first_day <- sim_data$hantavirus$first_reached_day
    data.frame(
      Country = names(infected),
      Active_Infected = round(as.numeric(infected), 2),
      Cumulative_Imported_Seeds = round(as.numeric(cumulative_imports), 2),
      First_Reached_Day = as.numeric(first_day[names(infected)]),
      stringsAsFactors = FALSE
    ) %>% filter(Active_Infected >= 0.01 | Cumulative_Imported_Seeds > 0 | !is.na(First_Reached_Day)) %>% arrange(desc(Active_Infected)) %>% head(20)
  }, rownames = FALSE)

  output$top_passenger_routes_table <- renderTable({
    req(sim_data$hantavirus$passenger_matrix_annual)
    m <- sim_data$hantavirus$passenger_matrix_annual
    idx <- which(m > 0, arr.ind = TRUE)
    df <- data.frame(
      Origin = rownames(m)[idx[, 1]],
      Destination = colnames(m)[idx[, 2]],
      Annual_Passengers = as.numeric(m[idx]),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Annual_Passengers)) %>% head(15)
    df$Annual_Passengers <- format_big(df$Annual_Passengers)
    df
  }, rownames = FALSE)



  output$spread_diagnostic_table <- renderTable({
    req(sim_data$hantavirus$country_history, sim_data$hantavirus$new_country_import_history, sim_data$hantavirus$expected_import_history)
    history <- sim_data$hantavirus$country_history
    imports <- sim_data$hantavirus$new_country_import_history
    expected_imports <- sim_data$hantavirus$expected_import_history
    selected_day <- min(max(input$time_slider, 0), nrow(history) - 1)
    values <- history[selected_day + 1, ]
    imports_today <- imports[selected_day + 1, ]
    expected_today <- expected_imports[selected_day + 1, ]

    data.frame(
      Check = c(
        "Selected day",
        "Countries stored in country_history with >0 active infections",
        "Countries stored in country_history with >=0.01 active infections",
        "Countries stored in country_history with >=1 active infection",
        "Countries eligible for map/bar display using current threshold >=0.01",
        "Imported seed cases stored today",
        "Expected imported seed pressure today",
        "Cumulative imported seed cases stored",
        "Maximum active infections in any non-starting country today",
        "Total annual passengers out of starting country"
      ),
      Value = c(
        selected_day,
        sum(values > 0, na.rm = TRUE),
        sum(values >= 0.01, na.rm = TRUE),
        sum(values >= 1, na.rm = TRUE),
        sum(values >= 0.01, na.rm = TRUE),
        format_big(sum(imports_today, na.rm = TRUE), 4),
        format_big(sum(expected_today, na.rm = TRUE), 4),
        format_big(sum(imports, na.rm = TRUE), 4),
        format_big(max(values[names(values) != input$starting_country], na.rm = TRUE), 4),
        format_big(sum(sim_data$hantavirus$passenger_matrix_annual[input$starting_country, ], na.rm = TRUE), 0)
      ),
      Interpretation = c(
        "Map and country charts show this day only.",
        "If this is >1 but map shows one country, the failure is visual filtering or map update.",
        "This uses the same threshold as the map/bar chart.",
        "This is a stricter epidemiological display threshold.",
        "If this is >1 and the map still shows one marker, the issue is in leafletProxy/update_country_map.",
        "If zero while expected pressure is high, conversion from expected imports to active infections failed.",
        "If high while stored imports are zero, the calculation is being lost before storage.",
        "If zero at the end, no international importation has entered country_history.",
        "If this is high but countries reached is 1, the issue is representation or row/column naming.",
        "Confirms whether the traffic matrix has outgoing routes from the starting country."
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$import_diagnostic_table <- renderTable({
    req(sim_data$hantavirus$country_history, sim_data$hantavirus$new_country_import_history, sim_data$hantavirus$expected_import_history)
    history <- sim_data$hantavirus$country_history
    imports <- sim_data$hantavirus$new_country_import_history
    expected_imports <- sim_data$hantavirus$expected_import_history
    selected_day <- min(max(input$time_slider, 0), nrow(history) - 1)

    values <- history[selected_day + 1, ]
    imports_today <- imports[selected_day + 1, ]
    expected_today <- expected_imports[selected_day + 1, ]
    cumulative_imports <- colSums(imports[seq_len(selected_day + 1), , drop = FALSE], na.rm = TRUE)

    df <- data.frame(
      Country = names(values),
      Active_Infected = as.numeric(values),
      Imported_Seeds_Today = as.numeric(imports_today),
      Expected_Import_Pressure_Today = as.numeric(expected_today),
      Cumulative_Imported_Seeds = as.numeric(cumulative_imports),
      First_Reached_Day = as.numeric(sim_data$hantavirus$first_reached_day[names(values)]),
      stringsAsFactors = FALSE
    ) %>%
      filter(Active_Infected >= 0.01 | Imported_Seeds_Today > 0 | Expected_Import_Pressure_Today > 0 | Cumulative_Imported_Seeds > 0 | !is.na(First_Reached_Day)) %>%
      arrange(desc(Active_Infected), desc(Cumulative_Imported_Seeds), desc(Expected_Import_Pressure_Today)) %>%
      head(20)

    if (nrow(df) == 0) {
      return(data.frame(Message = "No active infection or import signal at the selected day"))
    }

    df$Active_Infected <- round(df$Active_Infected, 4)
    df$Imported_Seeds_Today <- round(df$Imported_Seeds_Today, 4)
    df$Expected_Import_Pressure_Today <- round(df$Expected_Import_Pressure_Today, 4)
    df$Cumulative_Imported_Seeds <- round(df$Cumulative_Imported_Seeds, 4)
    df
  }, rownames = FALSE)
  output$evolution_stacked_area <- renderPlotly({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    if (is.null(h$cost_profile$dynamic_module) || !identical(h$cost_profile$dynamic_module, "active")) {
      return(plot_ly() %>% layout(title = "Dynamic replacement module is switched off."))
    }
    req(h$variant_frequency_history)
    vf <- h$variant_frequency_history %>% mutate(Variant = paste("Variant", variant_id), FrequencyPct = frequency * 100)
    if (nrow(vf) == 0) return(plot_ly() %>% layout(title = "No dynamic frequency data available"))
    p <- plot_ly()
    for (v in unique(vf$Variant)) {
      d <- vf %>% filter(Variant == v)
      p <- p %>% add_trace(data = d, x = ~day, y = ~FrequencyPct, name = v, type = "scatter", mode = "lines", stackgroup = "one", hovertemplate = paste0("<b>", v, "</b><br>Day: %{x}<br>Frequency: %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(title = "Dynamic Frequency Over Time (Stacked Area)", xaxis = list(title = "Days"), yaxis = list(title = "Frequency (%)", range = c(0, 100))) %>% config(displayModeBar = TRUE)
  })

  output$fitness_evolution <- renderPrint({
    req(sim_data$hantavirus$data)
    h <- sim_data$hantavirus
    if (is.null(h$cost_profile$dynamic_module) || !identical(h$cost_profile$dynamic_module, "active")) {
      cat("=== DYNAMIC MODULE DIAGNOSTIC ===\n\n")
      cat("The dynamic replacement module is switched off for this run.\n")
      return(invisible(NULL))
    }
    variants <- h$variants_emerged
    candidates <- h$candidate_mutations
    r0h <- h$R0_history
    cat("=== DYNAMIC MODULE ANALYSIS ===\n\n")
    cat(sprintf("Initial R0: %.2f\n", input$R0))
    cat(sprintf("Final effective R0: %.2f\n", tail(r0h$effective_R0, 1)))
    cat(sprintf("Macro-effective opportunity rate: %.8f\n", ifelse(is.null(sim_data$hantavirus$cost_profile$dynamic_rate), input$mutation_rate_per_replication, as.numeric(sim_data$hantavirus$cost_profile$dynamic_rate))))
    cat(sprintf("Population-level replacement opportunity multiplier: %d\n", ifelse(is.null(sim_data$hantavirus$cost_profile$dynamic_effective_targets), input$effective_mutation_targets, as.integer(sim_data$hantavirus$cost_profile$dynamic_effective_targets))))
    cat(sprintf("Dynamic module calibration: %s\n", variant_calibration_values(ifelse(is.null(sim_data$hantavirus$cost_profile$dynamic_calibration), input$variant_emergence_calibration, sim_data$hantavirus$cost_profile$dynamic_calibration))$label))
    cat(sprintf("Candidate macro-events generated: %d\n", nrow(candidates)))
    cat(sprintf("Established replacement events: %d\n", max(0, nrow(variants) - 1)))
    if (nrow(candidates) > 0) {
      cat("\nCandidate macro-event classes:\n")
      print(table(candidates$mutation_class))
      cat("\nEstablished by class:\n")
      print(table(candidates$mutation_class, candidates$established))
    }
    if (nrow(variants) > 1) {
      cat("\nEstablished variants:\n")
      for (i in 2:nrow(variants)) {
        cat(sprintf("Variant %d | Day %d | Origin: %s | Class: %s | Advantage: %.2f%% | Variant R0: %.2f | Establishment p: %.3f\n",
                    variants$variant_id[i], variants$day[i], variants$origin_country[i], variants$mutation_class[i],
                    variants$fitness_advantage[i], variants$R0_value[i], variants$establishment_probability[i]))
      }
    } else {
      cat("\nNo replacement event established in this run. Candidate macro-events may still have occurred.\n")
    }
    cat("\nDynamic diagnostics:\n")
    print(calculate_dynamic_diagnostics(h, input$R0, ifelse(is.null(h$cost_profile$dynamic_max_R0_multiplier), get_dynamic_config()$max_multiplier, suppressWarnings(as.numeric(h$cost_profile$dynamic_max_R0_multiplier)))))
    cat("\nInterpretation:\n")
    cat("- This is a population-level variant replacement model.\n")
    cat("- Candidate generation depends on incident activity and the macro-rate input.\n")
    cat("- Most candidate macro-events do not establish in the population-level model.\n")
    cat("- Effective R0 is frequency-weighted, not multiplied cumulatively.
- Adaptive-space saturation reduces expressed advantage as effective R0 approaches the scenario cap.\n")
  })

  output$dynamic_diagnostics_table <- renderTable({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    if (is.null(h$cost_profile$dynamic_module) || !identical(h$cost_profile$dynamic_module, "active")) {
      return(data.frame(Metric = "Dynamic module status", Value = "Not active for this run", stringsAsFactors = FALSE))
    }
    calculate_dynamic_diagnostics(h, isolate(input$R0), ifelse(is.null(h$cost_profile$dynamic_max_R0_multiplier), get_dynamic_config()$max_multiplier, suppressWarnings(as.numeric(h$cost_profile$dynamic_max_R0_multiplier))))
  }, rownames = FALSE)

  output$variant_plausibility_table <- renderTable({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    if (is.null(h$cost_profile$dynamic_module) || !identical(h$cost_profile$dynamic_module, "active")) {
      return(data.frame(Metric = "Dynamic module status", Value = "Not active for this run", stringsAsFactors = FALSE))
    }
    req(h$variants_emerged, h$candidate_mutations)
    variants <- h$variants_emerged
    candidates <- h$candidate_mutations
    established <- max(0, nrow(variants) - 1)
    days <- max(1, input$simulation_days)
    established_per_year <- established / days * 365
    candidate_per_year <- nrow(candidates) / days * 365
    plausibility_band <- dplyr::case_when(
      established_per_year == 0 ~ "No established replacement variant in this run",
      established_per_year <= 1 ~ "Low frequency; plausible for many infections or short simulations",
      established_per_year <= 5 ~ "Comparable to high-incidence pandemic-scale replacement dynamics",
      established_per_year <= 12 ~ "High; compatible only with intense transmission or permissive assumptions",
      TRUE ~ "Very high; probably over-generating established variants"
    )
    data.frame(
      Metric = c("Candidate macro-events generated", "Established replacement events", "Candidate macro-events per simulated year", "Established events per simulated year", "Plausibility interpretation", "Calibration note"),
      Value = c(format_big(nrow(candidates)), format_big(established), round(candidate_per_year, 1), round(established_per_year, 2), plausibility_band, "Large-scale replacement variants should be rare relative to candidate mutations; use this as a calibration diagnostic."),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)

  output$selective_sweep_table <- renderTable({
    req(sim_data$hantavirus)
    h <- sim_data$hantavirus
    if (is.null(h$cost_profile$dynamic_module) || !identical(h$cost_profile$dynamic_module, "active")) {
      return(data.frame(Variant = "Dynamic module not active for this run", Emerged_Day = NA, Mutation_Class = NA, Advantage_Percent = NA, Variant_R0 = NA, Peak_Frequency_Percent = NA, Sweep_Type = "Not applicable"))
    }
    req(h$variants_emerged, h$variant_frequency_history)
    variants <- h$variants_emerged %>% filter(variant_id > 1)
    vf <- h$variant_frequency_history
    if (nrow(variants) == 0) {
      return(data.frame(Variant = "No established replacement events", Emerged_Day = NA, Mutation_Class = NA, Advantage_Percent = NA, Variant_R0 = NA, Peak_Frequency_Percent = NA, Sweep_Type = "Stable"))
    }
    peak_freq <- vf %>% group_by(variant_id) %>% summarise(Peak_Frequency_Percent = max(frequency, na.rm = TRUE) * 100, .groups = "drop")
    variants %>% left_join(peak_freq, by = "variant_id") %>% transmute(
      Variant = paste("Variant", variant_id), Emerged_Day = day, Origin_Country = origin_country, Mutation_Class = mutation_class,
      Advantage_Percent = round(fitness_advantage, 2), Variant_R0 = round(R0_value, 2), Peak_Frequency_Percent = round(Peak_Frequency_Percent, 1),
      Sweep_Type = ifelse(Peak_Frequency_Percent >= 50, "Selective sweep", "Partial expansion")
    )
  }, rownames = FALSE)

  output$plot_r0_comparison <- renderPlotly({
    req(sim_data$hantavirus$R0_history, sim_data$covid$R0_history)
    h <- sim_data$hantavirus$R0_history %>% mutate(Virus = "New Virus")
    c <- sim_data$covid$R0_history %>% mutate(Virus = "COVID-19 Omicron reference (origin: South Africa)")
    df <- bind_rows(h, c)
    plot_ly(df, x = ~day, y = ~effective_R0, color = ~Virus, type = "scatter", mode = "lines", hovertemplate = "<b>%{fullData.name}</b><br>Day: %{x}<br>Effective R0: %{y:.2f}<extra></extra>") %>%
      layout(title = "Effective R0 Over Time", xaxis = list(title = "Day"), yaxis = list(title = "Effective R0")) %>% config(displayModeBar = TRUE)
  })

  output$plot_cfr_comparison <- renderPlotly({
    req(sim_data$hantavirus$data, sim_data$covid$data)
    df <- data.frame(Virus = c("New Virus", "COVID-19 Omicron reference (origin: South Africa)"), CFR = c(input$mortality_rate, 1), stringsAsFactors = FALSE)
    plot_ly(df, x = ~Virus, y = ~CFR, type = "bar", marker = list(color = c("#E74C3C", "#27AE60")), hovertemplate = "<b>%{x}</b><br>CFR: %{y:.1f}%<extra></extra>") %>%
      layout(title = "Case Fatality Rate Comparison", xaxis = list(title = "Virus"), yaxis = list(title = "CFR (%)")) %>% config(displayModeBar = TRUE)
  })

  output$r0_details <- renderPrint({
    req(sim_data$hantavirus$R0_history, sim_data$covid$R0_history)
    cat("=== R0 CALCULATION DETAILS ===\n\n")
    cat("New Virus:\n")
    cat(sprintf("  Initial R0 = %.2f\n", input$R0))
    cat(sprintf("  Infectious window = %d days\n", input$infectious_period_days))
    cat(sprintf("  Initial beta = %.4f/day\n", input$R0 / input$infectious_period_days))
    cat(sprintf("  Final effective R0 = %.2f\n", tail(sim_data$hantavirus$R0_history$effective_R0, 1)))
    cat(sprintf("  Established variants = %d\n", sim_data$hantavirus$total_variants))
    cat("\nCOVID-19 Omicron Reference:\n")
    cat("  Initial R0 = 4.25\n")
    cat("  Infectious period = 5 days\n")
    cat("  Initial beta = 0.8500/day\n")
    cat(sprintf("  Final effective R0 = %.2f\n", tail(sim_data$covid$R0_history$effective_R0, 1)))
    cat("\nNotes:\n")
    cat("  Effective R0 is the weighted average of established variant R0 values.\n")
    cat("  The COVID-19 reference disables further variant evolution.\n")
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("sir_evolution_simulation_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(sim_data$hantavirus$data, sim_data$covid$data)
      h <- sim_data$hantavirus$data; c <- sim_data$covid$data
      combined_data <- data.frame(
        Day = c(h$time, c$time),
        Virus = rep(c("New Virus", "COVID-19 Omicron reference (origin: South Africa)"), times = c(nrow(h), nrow(c))),
        Infected = c(h$I, c$I), Susceptible = c(h$S, c$S), Recovered = c(h$R, c$R), Deaths = c(h$D, c$D)
      )
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)