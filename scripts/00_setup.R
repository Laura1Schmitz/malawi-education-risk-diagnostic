#### ------------------------------------------------------------
#### 00_setup.R — packages, paths, global options
#### ------------------------------------------------------------

# 0) Housekeeping -------------------------------------------------------------
rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  warn = 1
)

# 1) Packages -----------------------------------------------------------------
pkgs <- c(
  # config + paths
  "yaml", "here",
  
  # data handling
  "haven", "readr", "dplyr", "tidyr", "stringr", "lubridate", "janitor", "openxlsx",
  
  # survey weights
  "survey",
  
  # spatial
  "sf", "tmap", "rnaturalearth", "rnaturalearthdata",
  
  # plots + tables
  "ggplot2", "scales", "knitr", "kableExtra"
)

install_if_missing <- function(x) {
  miss <- x[!x %in% rownames(installed.packages())]
  if (length(miss) > 0) install.packages(miss, dependencies = TRUE)
  invisible(TRUE)
}
install_if_missing(pkgs)

invisible(lapply(pkgs, library, character.only = TRUE))

# 2) Project root -------------------------------------------------------------
# Recommended: open the .Rproj in the repo root so here::here() resolves correctly.
root <- here::here()

required_dirs <- c("config", "data", "scripts", "outputs", "metadata")
missing_dirs <- required_dirs[!dir.exists(file.path(root, required_dirs))]
if (length(missing_dirs) > 0) {
  stop(
    "Project root check failed. Missing: ",
    paste(missing_dirs, collapse = ", "),
    "\nOpen the .Rproj file in the repo root, then rerun 00_setup.R.",
    call. = FALSE
  )
}

# 3) Read paths config --------------------------------------------------------
paths_file   <- file.path(root, "config", "paths.yml")
example_file <- file.path(root, "config", "paths_example.yml")

if (file.exists(paths_file)) {
  cfg <- yaml::read_yaml(paths_file)
  paths_file_used <- paths_file
} else if (file.exists(example_file)) {
  cfg <- yaml::read_yaml(example_file)
  paths_file_used <- example_file
  message(
    "Using config/paths_example.yml (no local paths.yml found).\n",
    "Create config/paths.yml to point to your local data directories."
  )
} else {
  stop(
    "No paths configuration found.\n",
    "Expected either config/paths.yml or config/paths_example.yml.",
    call. = FALSE
  )
}

message("cfg class: ", paste(class(cfg), collapse = ", "))
message("cfg is NULL: ", is.null(cfg))
print(cfg)
str(cfg)


# Normalize YAML keys (handles UTF-8 BOM + accidental whitespace)
if (is.null(cfg)) cfg <- list()
if (!is.null(names(cfg))) {
  names(cfg) <- trimws(names(cfg))
  names(cfg) <- gsub("\ufeff", "", names(cfg))  # remove BOM if present
}

message("Config loaded from: ", normalizePath(paths_file_used, winslash = "/", mustWork = FALSE))
message("Keys found in config: ", paste(names(cfg), collapse = ", "))


required_keys <- c("mics_data", "mics_gis")
optional_keys <- c("ihs_data")

missing_keys <- setdiff(required_keys, names(cfg))
if (length(missing_keys) > 0) {
  stop("Paths config is missing keys: ", paste(missing_keys, collapse = ", "), call. = FALSE)
}

# Ensure optional keys exist as NA if not provided
for (k in optional_keys) {
  if (!k %in% names(cfg)) cfg[[k]] <- NA_character_
}

# Convert relative paths to absolute paths (root-relative)
abs_path <- function(p) {
  if (is.null(p) || is.na(p) || !nzchar(p)) return(NA_character_)
  # Absolute Windows (C:\ or C:/) or Unix (/)
  is_abs <- grepl("^[A-Za-z]:[\\/]", p) || startsWith(p, "/")
  if (is_abs) p else file.path(root, p)
}

paths <- list(
  mics_data = abs_path(cfg$mics_data),
  mics_gis  = abs_path(cfg$mics_gis),
  ihs_data  = abs_path(cfg$ihs_data)
)

# 4) Output directories (project-wide) ---------------------------------------
OUT_DIR     <- file.path(root, "outputs")
OUT_TABLES  <- file.path(OUT_DIR, "tables")
OUT_MAPS    <- file.path(OUT_DIR, "maps")
OUT_FIGURES <- file.path(OUT_DIR, "figures")
OUT_OTHER   <- file.path(OUT_DIR, "other")

dir.create(OUT_DIR,     recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_MAPS,    recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_OTHER,   recursive = TRUE, showWarnings = FALSE)

# 5) Global constants ---------------------------------------------------------
PRIMARY_AGE_MIN <- 6
PRIMARY_AGE_MAX <- 13
HIGH_RISK_MIN_DOMAINS <- 2

# Verbosity flag (controls printing in downstream scripts)
VERBOSE <- FALSE

# 6) Helper functions ---------------------------------------------------------
write_csv_safe <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path)
}

# 7) Session info (useful for reproducibility) -------------------------------
session_info_path <- file.path(OUT_OTHER, "sessionInfo.txt")
writeLines(capture.output(sessionInfo()), session_info_path)

# 8) Input existence checks ---------------------------------------------------
# mics_data can be a file; mics_gis is expected to be a directory (GIS root).
missing_inputs <- c()

if (is.na(paths$mics_data) || !file.exists(paths$mics_data)) {
  missing_inputs <- c(missing_inputs, "mics_data")
}
if (is.na(paths$mics_gis) || !dir.exists(paths$mics_gis)) {
  missing_inputs <- c(missing_inputs, "mics_gis")
}

# Optional input: do not block execution
optional_missing <- character(0)
if (!is.na(paths$ihs_data) && nzchar(paths$ihs_data) && !file.exists(paths$ihs_data)) {
  optional_missing <- c(optional_missing, "ihs_data")
}

if (length(missing_inputs) > 0) {
  message("Missing required input(s): ", paste(missing_inputs, collapse = ", "))
  message("Raw data are not tracked. Update config/paths.yml (recommended) or config/paths_example.yml.")
}

if (length(optional_missing) > 0) {
  message("Optional input(s) not found: ", paste(optional_missing, collapse = ", "))
}

# 9) Startup summary ----------------------------------------------------------
message("Config loaded from: ", normalizePath(paths_file_used, winslash = "/", mustWork = FALSE))
message("00_setup.R completed.")
