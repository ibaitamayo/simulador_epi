source("tests/regression/helpers/extract_kpis.R")
source("tests/regression/helpers/run_single_scenario.R")
source("tests/regression/helpers/compare_results.R")

scenario_dir <- "tests/regression/scenarios"
expected_dir <- "tests/regression/expected"

scenario_files <- list.files(
  scenario_dir,
  pattern = "\\.json$",
  full.names = TRUE
)

all_results <- list()

cat("\n==============================\n")
cat("RUNNING REGRESSION SUITE\n")
cat("==============================\n\n")

for (scenario_file in scenario_files) {

  scenario_name <- tools::file_path_sans_ext(
    basename(scenario_file)
  )

  cat("\n---------------------------------\n")
  cat("Scenario:", scenario_name, "\n")
  cat("---------------------------------\n")

  observed <- run_single_scenario(
    scenario_file
  )

  expected_file <- file.path(
    expected_dir,
    paste0(scenario_name, "_summary.csv")
  )

  if (!file.exists(expected_file)) {

    cat("Expected baseline missing -> creating\n")

    write.csv(
      observed,
      expected_file,
      row.names = FALSE
    )

    comparison <- data.frame(
      metric = "baseline_created",
      observed = NA,
      expected = NA,
      difference = NA,
      status = "BASELINE_CREATED",
      stringsAsFactors = FALSE
    )

  } else {

    expected <- read.csv(expected_file)

    comparison <- compare_results(
      observed,
      expected
    )
  }

  comparison$scenario <- scenario_name

  all_results[[scenario_name]] <- comparison
}

final_results <- do.call(
  rbind,
  all_results
)

rownames(final_results) <- NULL

cat("\n==============================\n")
cat("FINAL SUMMARY\n")
cat("==============================\n\n")

print(final_results)

write.csv(
  final_results,
  "tests/regression/reports/regression_results.csv",
  row.names = FALSE
)

cat("\nSaved:\n")
cat("tests/regression/reports/regression_results.csv\n")
