# ------------------------------------------------------------
# 01_import_mics.R — Import MICS extract + basic metadata
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Inputs ----
mics_path <- paths$mics_data
if (is.na(mics_path) || !file.exists(mics_path)) {
  stop("MICS data file not found: ", mics_path, call. = FALSE)
}

ext <- tolower(tools::file_ext(mics_path))
mics_raw <- switch(
  ext,
  "dta" = haven::read_dta(mics_path),
  "sav" = haven::read_sav(mics_path),
  stop("Unsupported file type for mics_data: ", ext, call. = FALSE)
)

mics <- janitor::clean_names(mics_raw)

# ---- Variable name map (original → cleaned) ----
name_map <- tibble::tibble(
  original_name = names(mics_raw),
  cleaned_name  = names(mics)
)

dir.create(file.path(root, "metadata"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(name_map, file.path(root, "metadata", "mics_name_map.csv"))

# ---- Candidate field presence (quick QA) ----
candidate_fields <- list(
  hh_id      = c("hhid", "hh1", "hh2", "hh01", "cluster", "psu", "ea"),
  child_line = c("ln", "line", "hl1", "chline"),
  weight     = c("chwt", "hhwt", "wmweight", "weight"),
  district   = c("hh8", "district", "adm2", "district_name"),
  region     = c("hh7", "region", "adm1", "region_name"),
  urban      = c("hh6", "urban", "urb_rur", "residence")
)

present_candidates <- lapply(candidate_fields, function(v) v[v %in% names(mics)])

cand_lines <- c("Candidate fields present in MICS extract:", "")
for (nm in names(present_candidates)) {
  cand_lines <- c(
    cand_lines,
    paste0(
      "- ", nm, ": ",
      if (length(present_candidates[[nm]]) == 0) "<none found>" else paste(present_candidates[[nm]], collapse = ", ")
    )
  )
}
writeLines(cand_lines, file.path(OUT_OTHER, "mics_candidate_fields.txt"))

# ---- Variable inventory (labels + types) ----
var_labels <- sapply(mics_raw, function(x) attr(x, "label"))
var_types  <- sapply(mics, function(x) class(x)[1])

inv <- tibble::tibble(
  var_name = names(mics),
  type     = unname(var_types),
  label    = unname(var_labels)[match(names(mics), names(var_labels))]
)

readr::write_csv(inv, file.path(root, "metadata", "mics_variable_inventory.csv"))

# ---- Missingness summary (top 50) ----
miss_share <- sapply(mics, function(x) mean(is.na(x)))
miss_tbl <- tibble::tibble(
  var_name = names(mics),
  missing_share = as.numeric(miss_share)
) |>
  dplyr::arrange(dplyr::desc(missing_share))

readr::write_csv(
  dplyr::slice_head(miss_tbl, n = 50),
  file.path(OUT_TABLES, "mics_missingness_top.csv")
)

# ---- Optional: import cluster GPS (for linkage only) ----
# paths$mics_gis should point to the MICS GIS root folder (MWI_2019_MICS_v01_M)
mics_gis <- NULL
gis_root <- paths$mics_gis

xwalk_shp <- file.path(
  gis_root,
  "GPS Datasets",
  "MalawiMICS2019-20GPS",
  "MalawiMICS2019-20GPS.shp"
)

if (!is.na(gis_root) && dir.exists(gis_root) && file.exists(xwalk_shp)) {
  mics_gis <- sf::st_read(xwalk_shp, quiet = TRUE) |>
    janitor::clean_names() |>
    sf::st_make_valid()
  
  gis_inv <- tibble::tibble(
    var_name = names(sf::st_drop_geometry(mics_gis)),
    type     = sapply(sf::st_drop_geometry(mics_gis), function(x) class(x)[1])
  )
  readr::write_csv(gis_inv, file.path(root, "metadata", "mics_gis_variable_inventory.csv"))
}

# ---- Save interim objects ----
dir.create(file.path(root, "data", "interim"), recursive = TRUE, showWarnings = FALSE)
saveRDS(mics, file.path(root, "data", "interim", "mics_raw_clean_names.rds"))

if (!is.null(mics_gis)) {
  saveRDS(mics_gis, file.path(root, "data", "interim", "mics_gis_clusters.rds"))
}

message("01_import_mics.R completed.")
