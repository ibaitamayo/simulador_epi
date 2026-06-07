AGE_DISTRIBUTION_RDS_FILE <- "inst/data/country_age_distribution_wpp2024_6groups.rds"
AGE_DISTRIBUTION_CSV_FILE <- "inst/data/country_age_distribution_wpp2024_6groups.csv"
WORLD_COUNTRY_POLYGONS_RDS_FILE <- "inst/data/world_countries_simplified.rds"

# Data-loading and file-location helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

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

if (!exists("APP_DIR", envir = .GlobalEnv)) {
  APP_DIR <- detect_current_app_dir()
}


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







