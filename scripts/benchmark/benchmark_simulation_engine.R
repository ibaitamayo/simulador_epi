benchmark_engine <- function(app_file) {

  env <- new.env(parent = globalenv())
  source(app_file, local = env)

  t <- system.time({

    res <- env$run_simulation_age_adjusted(
      R0_target = 2.5,
      infectious_period_days = 7,
      mortality_rate = 0.01,
      N = 1000000,
      I0 = 100,
      days = 180,
      mutation_rate_per_replication = 0,
      effective_mutation_targets = 1,
      variant_emergence_calibration = 1,
      maximum_variant_R0_multiplier = 1,
      enable_evolution = FALSE,
      starting_country = "Spain",
      air_travel_scenario = 1,
      import_establishment_probability = 1,
      infectiousness_profile = "mid",
      rng_seed = 123,
      neutral_age_weights = TRUE,
      age_cfr_scale = 1,
      containment_schedule = NULL,
      dynamic_adaptive_saturation = TRUE,
      dynamic_saturation_exponent = 1,
      age_distribution_mode = "country_specific",
      disease_compartment_model = "SEIRD",
      exposed_period_days = 4
    )

  })

  data.frame(
    app = app_file,
    countries = length(env$COUNTRIES_LIST),
    elapsed = unname(t["elapsed"]),
    user = unname(t["user.self"]),
    system = unname(t["sys.self"])
  )
}

res <- rbind(
  benchmark_engine("app_epidemiologic_v17_academic_freeze.R"),
  benchmark_engine("app_epidemiologic_v18_226_country_prototype.R")
)

print(res)
