# ------------------------------------------------------------
# 05_access_progression.R — Access (attendance) and progression (over-age)
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Inputs ----
ab <- readRDS(file.path(root, "data", "processed", "analysis_base.rds"))

req <- c(
  "cluster_id", "strata", "weight",
  "school_att", "age_years", "grade_cur",
  "sex_cat", "wealth_q5", "region", "urban"
)
miss <- setdiff(req, names(ab))
if (length(miss) > 0) {
  stop("Missing required variables: ", paste(miss, collapse = ", "), call. = FALSE)
}

# ---- Helpers ----
# Convert common binary codings to 0/1/NA
to_binary01 <- function(x) {
  x0 <- x
  if (inherits(x0, "haven_labelled")) x0 <- unclass(x0)
  if (is.logical(x0)) return(ifelse(is.na(x0), NA_real_, as.numeric(x0)))
  if (is.character(x0)) x0 <- suppressWarnings(as.numeric(x0))
  
  ifelse(
    is.na(x0), NA_real_,
    dplyr::case_when(
      x0 %in% c(1) ~ 1,        # yes
      x0 %in% c(0, 2) ~ 0,     # no (or already 0)
      TRUE ~ NA_real_
    )
  )
}

# Safe subgroup estimation using subset() + svymean()
estimate_by_group <- function(des, outcome, byvar, min_n = 25) {
  v <- des$variables[[byvar]]
  if (is.null(v)) stop("byvar not found in design: ", byvar, call. = FALSE)
  
  lvls <- sort(unique(as.character(v)))
  lvls <- lvls[!is.na(lvls) & lvls != "NA"]
  
  rows <- lapply(lvls, function(g) {
    dsg <- subset(des, as.character(des$variables[[byvar]]) == g)
    n_unw <- nrow(dsg$variables)
    
    if (n_unw < min_n) {
      return(dplyr::tibble(
        outcome = outcome, dimension = byvar, group = g,
        n_unweighted = n_unw, mean = NA_real_, se = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_
      ))
    }
    
    fml <- stats::as.formula(paste0("~", outcome))
    est <- try(survey::svymean(fml, dsg, na.rm = TRUE), silent = TRUE)
    
    if (inherits(est, "try-error")) {
      return(dplyr::tibble(
        outcome = outcome, dimension = byvar, group = g,
        n_unweighted = n_unw, mean = NA_real_, se = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_
      ))
    }
    
    m  <- as.numeric(stats::coef(est)[1])
    se <- as.numeric(survey::SE(est)[1])
    
    ci <- try(suppressWarnings(stats::confint(est)), silent = TRUE)
    if (!inherits(ci, "try-error") && length(ci) >= 2) {
      ci_low  <- as.numeric(ci[1])
      ci_high <- as.numeric(ci[2])
    } else {
      ci_low  <- m - 1.96 * se
      ci_high <- m + 1.96 * se
    }
    
    dplyr::tibble(
      outcome = outcome, dimension = byvar, group = g,
      n_unweighted = n_unw, mean = m, se = se,
      ci_low = ci_low, ci_high = ci_high
    )
  })
  
  dplyr::bind_rows(rows)
}

# ---- Analysis frame ----
df <- dplyr::tibble(
  cluster_id = to_num_safe(ab$cluster_id),
  strata     = to_num_safe(ab$strata),
  weight     = to_num_safe(ab$weight),
  
  school_att = to_binary01(ab$school_att),
  age_years  = to_num_safe(ab$age_years),
  grade_cur  = to_num_safe(ab$grade_cur),
  
  wealth_q5  = factor(to_chr_safe(ab$wealth_q5)),
  sex_cat    = factor(to_chr_safe(ab$sex_cat)),
  region     = factor(to_chr_safe(ab$region)),
  urban      = factor(to_chr_safe(ab$urban))
) |>
  dplyr::filter(
    !is.na(cluster_id),
    !is.na(strata),
    !is.na(weight), weight > 0
  ) |>
  dplyr::mutate(
    # Over-age is defined only for children currently attending with non-missing age and grade
    over_age_universe = school_att == 1 & !is.na(age_years) & !is.na(grade_cur),
    over_age = dplyr::if_else(
      over_age_universe,
      as.numeric(age_years >= (grade_cur + 6)),
      NA_real_
    )
  )

# ---- Survey design ----
design_all <- survey::svydesign(
  ids = ~cluster_id,
  strata = ~strata,
  weights = ~weight,
  data = df,
  nest = TRUE
)

design_overage <- subset(design_all, over_age_universe)

# ---- National estimates ----
nat <- dplyr::bind_rows(
  {
    est <- survey::svymean(~school_att, design_all, na.rm = TRUE)
    ci  <- suppressWarnings(stats::confint(est))
    dplyr::tibble(
      outcome = "school_att",
      universe = "all_children",
      n_unweighted = nrow(design_all$variables),
      mean = as.numeric(stats::coef(est)[1]),
      se = as.numeric(survey::SE(est)[1]),
      ci_low = as.numeric(ci[1]),
      ci_high = as.numeric(ci[2])
    )
  },
  {
    est <- survey::svymean(~over_age, design_overage, na.rm = TRUE)
    ci  <- suppressWarnings(stats::confint(est))
    dplyr::tibble(
      outcome = "over_age",
      universe = "currently_attending_with_age_and_grade",
      n_unweighted = nrow(design_overage$variables),
      mean = as.numeric(stats::coef(est)[1]),
      se = as.numeric(survey::SE(est)[1]),
      ci_low = as.numeric(ci[1]),
      ci_high = as.numeric(ci[2])
    )
  }
)

# ---- Subgroup profiles ----
dims <- c("wealth_q5", "sex_cat", "region", "urban")

profiles_att <- dplyr::bind_rows(lapply(dims, function(byv) {
  estimate_by_group(design_all, "school_att", byv, min_n = 25)
})) |>
  dplyr::mutate(universe = "all_children")

profiles_ovg <- dplyr::bind_rows(lapply(dims, function(byv) {
  estimate_by_group(design_overage, "over_age", byv, min_n = 25)
})) |>
  dplyr::mutate(universe = "currently_attending_with_age_and_grade")

profiles <- dplyr::bind_rows(profiles_att, profiles_ovg)

# ---- Metadata ----
meta <- dplyr::tibble(
  n_all_children_unweighted = nrow(design_all$variables),
  n_over_age_universe_unweighted = nrow(design_overage$variables),
  share_over_age_universe = nrow(design_overage$variables) / nrow(design_all$variables),
  share_grade_nonmiss_overall = mean(!is.na(df$grade_cur)),
  share_attendance_nonmiss_overall = mean(!is.na(df$school_att))
)

if (VERBOSE) {
  print(nat)
  print(meta)
}

# ---- Save outputs ----
readr::write_csv(nat,      file.path(OUT_TABLES, "access_progression_national.csv"))
readr::write_csv(profiles, file.path(OUT_TABLES, "access_progression_profiles.csv"))
readr::write_csv(meta,     file.path(OUT_TABLES, "access_progression_meta.csv"))

openxlsx::write.xlsx(nat,      file.path(OUT_TABLES, "access_progression_national.xlsx"), overwrite = TRUE)
openxlsx::write.xlsx(profiles, file.path(OUT_TABLES, "access_progression_profiles.xlsx"), overwrite = TRUE)
openxlsx::write.xlsx(meta,     file.path(OUT_TABLES, "access_progression_meta.xlsx"), overwrite = TRUE)

message("05_access_progression.R completed.")
