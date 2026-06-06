compare_results <- function(
    observed,
    expected,
    numeric_tolerance = 0.005,
    peak_day_tolerance = 1) {

  stopifnot(nrow(observed) == 1)
  stopifnot(nrow(expected) == 1)

  results <- list()

  add_row <- function(metric,
                      observed_value,
                      expected_value,
                      difference = NA_real_,
                      status = "PASS") {

    data.frame(
      metric = metric,
      observed = as.character(observed_value),
      expected = as.character(expected_value),
      difference = difference,
      status = status,
      stringsAsFactors = FALSE
    )
  }

  # ---- Numeric metrics ----
  numeric_metrics <- c(
    "peak_active_population",
    "cumulative_deaths",
    "cumulative_recovered",
    "exposed_peak"
  )

  for (m in numeric_metrics) {

    obs <- as.numeric(observed[[m]])
    exp <- as.numeric(expected[[m]])

    pct_diff <- if (is.na(exp) || exp == 0) {
      NA_real_
    } else {
      abs(obs - exp) / abs(exp)
    }

    pass <- is.na(pct_diff) || pct_diff <= numeric_tolerance

    results[[m]] <- add_row(
      metric = m,
      observed_value = obs,
      expected_value = exp,
      difference = pct_diff,
      status = ifelse(pass, "PASS", "FAIL")
    )
  }

  # ---- Peak day ----
  peak_diff <- abs(
    as.numeric(observed$peak_day) -
    as.numeric(expected$peak_day)
  )

  results[["peak_day"]] <- add_row(
    metric = "peak_day",
    observed_value = observed$peak_day,
    expected_value = expected$peak_day,
    difference = peak_diff,
    status = ifelse(
      peak_diff <= peak_day_tolerance,
      "PASS",
      "FAIL"
    )
  )

  # ---- Exact metrics ----
  exact_metrics <- c(
    "countries_reached",
    "comparator_loaded",
    "warnings_count"
  )

  for (m in exact_metrics) {

    pass <- identical(
      observed[[m]],
      expected[[m]]
    )

    results[[m]] <- add_row(
      metric = m,
      observed_value = observed[[m]],
      expected_value = expected[[m]],
      difference = NA_real_,
      status = ifelse(pass, "PASS", "FAIL")
    )
  }

  # ---- Runtime ----
  runtime_diff <- abs(
    as.numeric(observed$runtime_seconds) -
    as.numeric(expected$runtime_seconds)
  )

  results[["runtime_seconds"]] <- add_row(
    metric = "runtime_seconds",
    observed_value = observed$runtime_seconds,
    expected_value = expected$runtime_seconds,
    difference = runtime_diff,
    status = "INFO"
  )

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  out
}
