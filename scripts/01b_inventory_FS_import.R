# ------------------------------------------------------------
# 01_inventory_and_fs_import.R — Enriched inventories + FS import
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Helper: enriched inventory ----
enrich_inventory <- function(df) {
  labs <- sapply(df, function(x) attr(x, "label"))
  has_value_labels <- sapply(df, function(x) !is.null(attr(x, "labels")))
  
  tibble::tibble(
    var_name = names(df),
    type = sapply(df, function(x) class(x)[1]),
    var_label = unname(labs),
    has_value_labels = as.logical(has_value_labels),
    n_unique = sapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
    missing_share = sapply(df, function(x) mean(is.na(x))),
    example_values = sapply(df, function(x) {
      v <- x[!is.na(x)]
      v <- head(v, 5)
      paste(as.character(v), collapse = ", ")
    })
  )
}

# ---- A) Enriched inventory for interim MICS extract (if present) ----
mics_rds <- file.path(root, "data", "interim", "mics_raw_clean_names.rds")
inv2_path <- file.path(root, "metadata", "mics_variable_inventory_enriched.csv")

if (file.exists(mics_rds)) {
  mics <- readRDS(mics_rds)
  
  labs <- sapply(mics, function(x) attr(x, "label"))
  has_value_labels <- sapply(mics, function(x) !is.null(attr(x, "labels")))
  
  inv2 <- tibble::tibble(
    var_name = names(mics),
    type = sapply(mics, function(x) class(x)[1]),
    var_label = unname(labs),
    has_value_labels = as.logical(has_value_labels),
    n_unique = sapply(mics, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
    example_values = sapply(mics, function(x) {
      v <- x[!is.na(x)]
      v <- head(v, 5)
      paste(as.character(v), collapse = ", ")
    })
  )
  
  dir.create(file.path(root, "metadata"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(inv2, inv2_path)
}

# ---- B) Enriched inventories for core module files (SPSS) ----
mics_dir <- dirname(paths$mics_data)

files <- c("bh.sav", "ch.sav", "fs.sav", "hh.sav", "hl.sav", "mn.sav", "tn.sav", "wm.sav")
paths_all <- file.path(mics_dir, files)
paths_all <- paths_all[file.exists(paths_all)]

inv_dir <- file.path(root, "metadata", "mics_inventories")
dir.create(inv_dir, recursive = TRUE, showWarnings = FALSE)

for (p in paths_all) {
  nm <- tools::file_path_sans_ext(basename(p))
  df <- haven::read_sav(p) |>
    janitor::clean_names()
  
  inv <- enrich_inventory(df)
  readr::write_csv(inv, file.path(inv_dir, paste0("inventory_", nm, "_enriched.csv")))
}

done <- tibble::tibble(
  file = basename(paths_all),
  path = paths_all
)
readr::write_csv(done, file.path(inv_dir, "mics_files_index.csv"))

# ---- C) Import FS module to interim (single source of truth for FS raw) ----
fs_path <- file.path(mics_dir, "fs.sav")
if (!file.exists(fs_path)) {
  stop("FS module not found: ", fs_path, call. = FALSE)
}

fs <- haven::read_sav(fs_path) |>
  janitor::clean_names()

dir.create(file.path(root, "data", "interim"), recursive = TRUE, showWarnings = FALSE)
saveRDS(fs, file.path(root, "data", "interim", "fs_raw_clean_names.rds"))

message("01_inventory_and_fs_import.R completed.")
