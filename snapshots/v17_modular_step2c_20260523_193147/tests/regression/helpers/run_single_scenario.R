run_single_scenario <- function(
    scenario_file,
    app_file = "app_epidemiologic_v17_academic_freeze.R") {

  if (!file.exists(scenario_file)) {
    stop("Scenario file not found: ", scenario_file)
  }

  if (!file.exists(app_file)) {
    stop("App file not found: ", app_file)
  }

  scenario <- jsonlite::read_json(
    scenario_file,
    simplifyVector = TRUE
  )

  message("------------------------------------------------")
  message("Running scenario: ", scenario$scenario_id)
  message("Country: ", scenario$country)

  start_time <- Sys.time()

  # Placeholder: here we will later call the real simulation
  # without launching the UI.

  sim_data <- list(
    hantavirus = list(
      data = data.frame(
        time = 1:10,
        I = c(1,2,4,8,12,10,7,5,2,1),
        R = c(0,0,1,2,4,7,10,15,20,25),
        D = c(0,0,0,0,1,2,3,4,5,5),
        E = c(2,4,6,10,8,5,3,2,1,0)
      ),
      first_reached_day = c(
        Spain = 1,
        France = 3,
        Portugal = 4
      ),
      warnings = NULL
    ),
    covid = list(
      data = TRUE
    )
  )

  runtime_seconds <- as.numeric(
    difftime(Sys.time(), start_time, units = "secs")
  )

  kpis <- extract_kpis(
    sim_data = sim_data,
    scenario_id = scenario$scenario_id,
    country = scenario$country,
    runtime_seconds = runtime_seconds
  )

  return(kpis)
}
