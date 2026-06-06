extract_kpis <- function(sim_data,
                         scenario_id = NA_character_,
                         country = "Spain",
                         runtime_seconds = NA_real_) {

  safe_max <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
    max(x, na.rm = TRUE)
  }

  safe_last <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
    tail(x, 1)
  }

  safe_peak_day <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_integer_)
    as.integer(which.max(x))
  }

  h <- sim_data$hantavirus

  if (is.null(h) || is.null(h$data)) {
    stop("sim_data$hantavirus$data not available")
  }

  df <- h$data

  peak_active_population <- safe_max(df$I)
  peak_day <- safe_peak_day(df$I)

  cumulative_deaths <- safe_last(df$D)
  cumulative_recovered <- safe_last(df$R)

  exposed_peak <- if ("E" %in% names(df)) {
    safe_max(df$E)
  } else {
    NA_real_
  }

  countries_reached <- NA_integer_

  if (!is.null(h$first_reached_day)) {
    countries_reached <- sum(!is.na(h$first_reached_day))
  }

  comparator_loaded <- !is.null(sim_data$covid$data)

  warnings_count <- 0L

  if (!is.null(h$warnings)) {
    warnings_count <- length(h$warnings)
  }

  data.frame(
    scenario_id = scenario_id,
    country = country,
    peak_active_population = peak_active_population,
    peak_day = peak_day,
    cumulative_deaths = cumulative_deaths,
    cumulative_recovered = cumulative_recovered,
    countries_reached = countries_reached,
    exposed_peak = exposed_peak,
    comparator_loaded = comparator_loaded,
    warnings_count = warnings_count,
    runtime_seconds = runtime_seconds,
    stringsAsFactors = FALSE
  )
}
