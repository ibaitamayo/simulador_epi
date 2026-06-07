# Transmission example configuration loader and validator.
# These objects load parameter templates only. They do not change the simulation engine.

read_transmission_config <- function(filename, strings_as_factors = FALSE) {
  path <- file.path("inst", "config", filename)

  if (!file.exists(path)) {
    stop("Missing configuration file: ", path, call. = FALSE)
  }

  read.csv(
    path,
    stringsAsFactors = strings_as_factors,
    na.strings = c("NA", "")
  )
}

load_transmission_examples <- function() {
  read_transmission_config("transmission_examples.csv")
}

load_transmission_example_metadata <- function() {
  read_transmission_config("transmission_example_metadata.csv")
}

load_transmission_families <- function() {
  read_transmission_config("transmission_families.csv")
}

load_transmission_example_references <- function() {
  read_transmission_config("transmission_example_references.csv")
}

validate_transmission_configuration <- function(verbose = TRUE) {
  examples <- load_transmission_examples()
  metadata <- load_transmission_example_metadata()
  families <- load_transmission_families()
  references <- load_transmission_example_references()

  required_examples <- c(
    "id",
    "label",
    "family",
    "compartment_model",
    "default_R0",
    "default_exposed_period_days",
    "default_infectious_period_days",
    "default_mortality_percent",
    "default_dynamic_scenario",
    "enabled",
    "review_status",
    "example_version"
  )

  required_metadata <- c(
    "id",
    "evidence_level",
    "confidence_level",
    "description",
    "scope",
    "assumptions",
    "limitations",
    "version",
    "last_review_date"
  )

  required_families <- c(
    "family_id",
    "family_label",
    "description"
  )

  required_references <- c(
    "example_id",
    "reference_id",
    "reference_label",
    "reference_type"
  )

  missing_examples <- setdiff(required_examples, names(examples))
  missing_metadata <- setdiff(required_metadata, names(metadata))
  missing_families <- setdiff(required_families, names(families))
  missing_references <- setdiff(required_references, names(references))

  if (length(missing_examples) > 0) {
    stop("Missing columns in transmission_examples.csv: ",
         paste(missing_examples, collapse = ", "), call. = FALSE)
  }

  if (length(missing_metadata) > 0) {
    stop("Missing columns in transmission_example_metadata.csv: ",
         paste(missing_metadata, collapse = ", "), call. = FALSE)
  }

  if (length(missing_families) > 0) {
    stop("Missing columns in transmission_families.csv: ",
         paste(missing_families, collapse = ", "), call. = FALSE)
  }

  if (length(missing_references) > 0) {
    stop("Missing columns in transmission_example_references.csv: ",
         paste(missing_references, collapse = ", "), call. = FALSE)
  }

  if (anyDuplicated(examples$id)) {
    stop("Duplicated example ids in transmission_examples.csv", call. = FALSE)
  }

  if (anyDuplicated(metadata$id)) {
    stop("Duplicated example ids in transmission_example_metadata.csv", call. = FALSE)
  }

  if (anyDuplicated(families$family_id)) {
    stop("Duplicated family ids in transmission_families.csv", call. = FALSE)
  }

  missing_family_ids <- setdiff(examples$family, families$family_id)

  if (length(missing_family_ids) > 0) {
    stop("Examples refer to undefined families: ",
         paste(missing_family_ids, collapse = ", "), call. = FALSE)
  }

  examples_without_metadata <- setdiff(examples$id, metadata$id)

  if (length(examples_without_metadata) > 0) {
    stop("Examples without metadata: ",
         paste(examples_without_metadata, collapse = ", "), call. = FALSE)
  }

  metadata_without_examples <- setdiff(metadata$id, examples$id)

  if (length(metadata_without_examples) > 0) {
    stop("Metadata without matching example: ",
         paste(metadata_without_examples, collapse = ", "), call. = FALSE)
  }

  if (nrow(references) > 0) {
    orphan_reference_ids <- setdiff(references$example_id, examples$id)

    if (length(orphan_reference_ids) > 0) {
      stop("References point to unknown examples: ",
           paste(orphan_reference_ids, collapse = ", "), call. = FALSE)
    }
  }

  invalid_compartment_models <- setdiff(
    unique(examples$compartment_model),
    c("SIRD", "SEIRD")
  )

  if (length(invalid_compartment_models) > 0) {
    stop("Invalid compartment_model values: ",
         paste(invalid_compartment_models, collapse = ", "), call. = FALSE)
  }

  if (verbose) {
    message("Transmission example configuration validated successfully.")
    message("Examples: ", nrow(examples))
    message("Families: ", nrow(families))
    message("Reference rows: ", nrow(references))
  }

  invisible(TRUE)
}

get_transmission_example <- function(example_id, examples = TRANSMISSION_EXAMPLES) {

  x <- examples[examples$id == example_id, ]

  if (nrow(x) != 1) {
    return(NULL)
  }

  x
}

apply_transmission_example <- function(
    session,
    example_id,
    examples = TRANSMISSION_EXAMPLES) {

  example_row <- get_transmission_example(
    example_id,
    examples
  )

  if (is.null(example_row)) {
    return(invisible(FALSE))
  }

  if (!is.na(example_row$default_R0)) {
    updateNumericInput(
      session,
      "R0",
      value = example_row$default_R0
    )
  }

  if (!is.na(example_row$default_exposed_period_days)) {
    updateNumericInput(
      session,
      "exposed_period_days",
      value = example_row$default_exposed_period_days
    )
  }

  if (!is.na(example_row$default_infectious_period_days)) {
    updateNumericInput(
      session,
      "infectious_period_days",
      value = example_row$default_infectious_period_days
    )
  }

  if (!is.na(example_row$default_mortality_percent)) {
    updateNumericInput(
      session,
      "mortality_rate",
      value = example_row$default_mortality_percent
    )
  }

  invisible(TRUE)
}

get_transmission_example_metadata <- function(
    example_id,
    metadata = TRANSMISSION_EXAMPLE_METADATA) {

  x <- metadata[
    metadata$id == example_id,
  ]

  if (nrow(x) != 1) {
    return(NULL)
  }

  x
}

is_transmission_example_custom <- function(
    example_id,
    r0,
    exposed_period,
    infectious_period,
    mortality_rate,
    examples = TRANSMISSION_EXAMPLES) {

  example_row <- get_transmission_example(
    example_id,
    examples
  )

  if (is.null(example_row)) {
    return(FALSE)
  }

  matches <- TRUE

  if (!is.na(example_row$default_R0)) {
    matches <- matches &&
      isTRUE(all.equal(
        as.numeric(r0),
        as.numeric(example_row$default_R0)
      ))
  }

  if (!is.na(example_row$default_exposed_period_days)) {
    matches <- matches &&
      isTRUE(all.equal(
        as.numeric(exposed_period),
        as.numeric(example_row$default_exposed_period_days)
      ))
  }

  if (!is.na(example_row$default_infectious_period_days)) {
    matches <- matches &&
      isTRUE(all.equal(
        as.numeric(infectious_period),
        as.numeric(example_row$default_infectious_period_days)
      ))
  }

  if (!is.na(example_row$default_mortality_percent)) {
    matches <- matches &&
      isTRUE(all.equal(
        as.numeric(mortality_rate),
        as.numeric(example_row$default_mortality_percent)
      ))
  }

  !matches
}

