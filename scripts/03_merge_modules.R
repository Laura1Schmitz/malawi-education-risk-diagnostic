# ------------------------------------------------------------
# 03_merge_modules.R — Merge FS outcomes with HL and HH modules
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Inputs ----
fs_clean <- readRDS(file.path(root, "data", "processed", "fs_clean.rds"))
fs_raw   <- readRDS(file.path(root, "data", "interim", "fs_raw_clean_names.rds"))

mics_dir <- dirname(paths$mics_data)

hl_raw <- haven::read_sav(file.path(mics_dir, "hl.sav")) |> janitor::clean_names()
hh_raw <- haven::read_sav(file.path(mics_dir, "hh.sav")) |> janitor::clean_names()

# ---- Keys ----
key_fs <- c("cluster_id", "hh_id", "child_line")
stopifnot(all(key_fs %in% names(fs_clean)))

cand_keys <- list(
  cluster = c("hh1", "uf1", "cluster", "psu"),
  hh      = c("hh2", "uf2", "household", "hh"),
  line    = c("ln", "hl1", "line", "hh3", "hl_ln", "member_line", "line_number")
)

hl_cluster_v <- pick_var(hl_raw, cand_keys$cluster, TRUE, "HL cluster key")
hl_hh_v      <- pick_var(hl_raw, cand_keys$hh,      TRUE, "HL household key")
hl_line_v    <- pick_var(hl_raw, cand_keys$line,    TRUE, "HL line key")

hh_cluster_v <- pick_var(hh_raw, cand_keys$cluster, TRUE, "HH cluster key")
hh_hh_v      <- pick_var(hh_raw, cand_keys$hh,      TRUE, "HH household key")

# ---- FS design variables (from FS raw) ----
cand_design <- list(
  weight = c("fsweight", "chweight", "hhweight", "wgt", "weight"),
  strata = c("hhstratum", "stratum", "strata", "hh6", "hh7", "stratum_id"),
  urban  = c("hh6", "urban", "urb", "area", "residence"),
  region = c("hh7", "region", "reg", "domain")
)

w_v <- pick_var(fs_raw, cand_design$weight, FALSE, "FS weight")
s_v <- pick_var(fs_raw, cand_design$strata, FALSE, "FS strata")
u_v <- pick_var(fs_raw, cand_design$urban,  FALSE, "FS urban")
r_v <- pick_var(fs_raw, cand_design$region, FALSE, "FS region")

# ---- Schooling variables (CB items stored in FS raw) ----
cand_fs_school <- list(
  age_years  = c("cb3", "age", "age_years"),
  school_att = c("cb7", "school_att", "attend_current_year"),
  grade_cur  = c("cb8b", "grade_cur", "current_grade")
)

cb_age_v   <- pick_var(fs_raw, cand_fs_school$age_years,  FALSE, "FS/CB age")
cb_att_v   <- pick_var(fs_raw, cand_fs_school$school_att, FALSE, "FS/CB attendance")
cb_grade_v <- pick_var(fs_raw, cand_fs_school$grade_cur,  FALSE, "FS/CB grade")

fs_design <- dplyr::tibble(
  cluster_id = to_num_safe(fs_raw$hh1),
  hh_id      = to_num_safe(fs_raw$hh2),
  child_line = to_num_safe(fs_raw$ln)
) |>
  dplyr::mutate(
    weight = if (!is.na(w_v)) to_num_safe(fs_raw[[w_v]]) else NA_real_,
    strata = if (!is.na(s_v)) to_num_safe(fs_raw[[s_v]]) else NA_real_,
    urban  = if (!is.na(u_v)) to_num_safe(fs_raw[[u_v]]) else NA_real_,
    region = if (!is.na(r_v)) to_num_safe(fs_raw[[r_v]]) else NA_real_,
    
    age_years  = if (!is.na(cb_age_v))   to_num_safe(fs_raw[[cb_age_v]])   else NA_real_,
    school_att = if (!is.na(cb_att_v))   to_num_safe(fs_raw[[cb_att_v]])   else NA_real_,
    grade_cur  = if (!is.na(cb_grade_v)) to_num_safe(fs_raw[[cb_grade_v]]) else NA_real_
  )

# ---- HL demographics ----
cand_hl <- list(sex = c("hl4", "sex", "csex"))
sex_v <- pick_var(hl_raw, cand_hl$sex, FALSE, "HL sex")

hl_child <- hl_raw |>
  dplyr::transmute(
    cluster_id = to_num_safe(.data[[hl_cluster_v]]),
    hh_id      = to_num_safe(.data[[hl_hh_v]]),
    child_line = to_num_safe(.data[[hl_line_v]]),
    sex_cat    = if (!is.na(sex_v)) to_num_safe(.data[[sex_v]]) else NA_real_
  )

# ---- HH wealth + controls ----
cand_hh <- list(
  wealth_score = c("wscore", "wealth_score", "wi", "wealthindex"),
  wealth_q5    = c("windex5", "wealth_quintile", "wi5"),
  wealth_q10   = c("windex10", "wealth_decile", "wi10"),
  hh_weight    = c("hhweight", "weight", "wgt")
)

ws_v  <- pick_var(hh_raw, cand_hh$wealth_score, FALSE, "HH wealth score")
w5_v  <- pick_var(hh_raw, cand_hh$wealth_q5,    FALSE, "HH wealth quintile")
w10_v <- pick_var(hh_raw, cand_hh$wealth_q10,   FALSE, "HH wealth decile")
hhw_v <- pick_var(hh_raw, cand_hh$hh_weight,    FALSE, "HH weight")

hh_vars <- hh_raw |>
  dplyr::transmute(
    cluster_id   = to_num_safe(.data[[hh_cluster_v]]),
    hh_id        = to_num_safe(.data[[hh_hh_v]]),
    wealth_score = if (!is.na(ws_v))  to_num_safe(.data[[ws_v]])  else NA_real_,
    wealth_q5    = if (!is.na(w5_v))  to_num_safe(.data[[w5_v]])  else NA_real_,
    wealth_q10   = if (!is.na(w10_v)) to_num_safe(.data[[w10_v]]) else NA_real_,
    hh_weight    = if (!is.na(hhw_v)) to_num_safe(.data[[hhw_v]]) else NA_real_
  )

# ---- Diagnostics: uniqueness of merge keys ----
dup_keys <- function(df, keys) {
  df |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") |>
    dplyr::summarise(
      n_dup = sum(.data$n > 1),
      max_n = max(.data$n, na.rm = TRUE)
    )
}

diag_unique <- dplyr::bind_rows(
  dplyr::mutate(dup_keys(fs_clean,  key_fs), label = "fs_clean"),
  dplyr::mutate(dup_keys(fs_design, key_fs), label = "fs_design"),
  dplyr::mutate(dup_keys(hl_child,  key_fs), label = "hl_child"),
  dplyr::mutate(dup_keys(hh_vars,   c("cluster_id", "hh_id")), label = "hh_vars")
)

# ---- Merge ----
analysis_base <- fs_clean |>
  dplyr::left_join(fs_design |> dplyr::distinct(), by = key_fs) |>
  dplyr::left_join(hl_child  |> dplyr::distinct(), by = key_fs) |>
  dplyr::left_join(hh_vars   |> dplyr::distinct(), by = c("cluster_id", "hh_id"))

# ---- Merge summary ----
merge_meta <- dplyr::tibble(
  n_fs_clean = nrow(fs_clean),
  
  hl_cluster_key = hl_cluster_v,
  hl_household_key = hl_hh_v,
  hl_line_key = hl_line_v,
  
  hh_cluster_key = hh_cluster_v,
  hh_household_key = hh_hh_v,
  
  fs_weight_var = if (!is.na(w_v)) w_v else NA_character_,
  fs_strata_var = if (!is.na(s_v)) s_v else NA_character_,
  fs_urban_var  = if (!is.na(u_v)) u_v else NA_character_,
  fs_region_var = if (!is.na(r_v)) r_v else NA_character_,
  
  fs_cb_age_var   = if (!is.na(cb_age_v)) cb_age_v else NA_character_,
  fs_cb_att_var   = if (!is.na(cb_att_v)) cb_att_v else NA_character_,
  fs_cb_grade_var = if (!is.na(cb_grade_v)) cb_grade_v else NA_character_,
  
  hh_wealth_score_var = if (!is.na(ws_v)) ws_v else NA_character_,
  hh_wealth_q5_var = if (!is.na(w5_v)) w5_v else NA_character_,
  hh_wealth_q10_var = if (!is.na(w10_v)) w10_v else NA_character_,
  
  merge_rate_sex_nonmiss = mean(!is.na(analysis_base$sex_cat), na.rm = TRUE),
  merge_rate_fs_age_nonmiss = mean(!is.na(analysis_base$age_years), na.rm = TRUE),
  merge_rate_fs_att_nonmiss = mean(!is.na(analysis_base$school_att), na.rm = TRUE),
  merge_rate_fs_grade_nonmiss = mean(!is.na(analysis_base$grade_cur), na.rm = TRUE),
  merge_rate_hh_wealthq5_nonmiss = mean(!is.na(analysis_base$wealth_q5), na.rm = TRUE)
)

# ---- Save outputs ----
dir.create(file.path(root, "data", "processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(analysis_base, file.path(root, "data", "processed", "analysis_base.rds"))

dir.create(file.path(root, "metadata"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(diag_unique, file.path(root, "metadata", "merge_key_uniqueness.csv"))
readr::write_csv(merge_meta,  file.path(root, "metadata", "merge_summary.csv"))

# ---- Required integrity checks ----
fs_clean2 <- readRDS(file.path(root, "data", "processed", "fs_clean.rds"))
ab <- readRDS(file.path(root, "data", "processed", "analysis_base.rds"))

stopifnot(nrow(fs_clean2) == nrow(ab))
stopifnot(identical(
  fs_clean2[, c("cluster_id","hh_id","child_line")],
  ab[,       c("cluster_id","hh_id","child_line")]
))

# ---- Optional diagnostics ----
if (VERBOSE) {
  u <- readr::read_csv(file.path(root, "metadata", "merge_key_uniqueness.csv"), show_col_types = FALSE)
  print(u)
  
  print(
    ab |>
      dplyr::summarise(
        share_age_nonmiss = mean(!is.na(age_years)),
        share_sex_nonmiss = mean(!is.na(sex_cat)),
        share_att_nonmiss = mean(!is.na(school_att)),
        share_grade_nonmiss = mean(!is.na(grade_cur)),
        share_wealth_nonmiss = mean(!is.na(wealth_q5) | !is.na(wealth_q10) | !is.na(wealth_score)),
        share_weight_nonmiss = mean(!is.na(weight)),
        share_strata_nonmiss = mean(!is.na(strata))
      )
  )
  
  print(table(fs_raw$cb7,  useNA = "ifany"))
  print(table(fs_raw$cb8b, useNA = "ifany"))
}

message("03_merge_modules.R completed.")
