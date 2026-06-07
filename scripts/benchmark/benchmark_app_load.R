benchmark_source <- function(file) {
  t <- system.time({
    env <- new.env(parent = globalenv())
    source(file, local = env)
  })
  
  data.frame(
    app_file = file,
    elapsed_seconds = unname(t["elapsed"]),
    user_seconds = unname(t["user.self"]),
    system_seconds = unname(t["sys.self"]),
    countries = length(env$COUNTRIES_LIST),
    polygons = nrow(env$WORLD_COUNTRY_POLYGONS$data)
  )
}

res <- rbind(
  benchmark_source("app_epidemiologic_v17_academic_freeze.R"),
  benchmark_source("app_epidemiologic_v18_226_country_prototype.R")
)

print(res)

write.csv(
  res,
  "docs/roadmap/benchmark_results/app_load_benchmark.csv",
  row.names = FALSE
)
