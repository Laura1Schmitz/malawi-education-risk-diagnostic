# ------------------------------------------------------------
# 04_risk_profiles.R — Risk profiles for Foundational Learning Skills
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Inputs ----
ab <- readRDS(file.path(root, "data", "processed", "analysis_base.rds"))

req <- c(
  "cluster_id", "strata", "weight",
  "fls_all", "fls_lit", "fls_num", "fls_any",
  "sex_cat", "wealth_q5", "region", "urban"
)
miss <- setdiff(req, names(ab))
if (length(miss) > 0) {
  stop("Missing required variables: ", paste(miss, collapse = ", "), call. = FALSE)
}

# ---- Analysis frame (tested universe) ----
df <- dplyr::tibble(
  cluster_id = to_num_safe(ab$cluster_id),
  strata     = to_num_safe(ab$strata),
  weight     = to_num_safe(ab$weight),
  
  fls_all    = to_num_safe(ab$fls_all),
  fls_lit    = to_num_safe(ab$fls_lit),
  fls_num    = to_num_safe(ab$fls_num),
  
  # fls_any may be logical / numeric / labelled
  fls_any_num = to_num_safe(ab$fls_any),
  
  wealth_q5  = factor(to_chr_safe(ab$wealth_q5)),
  sex_cat    = factor(to_chr_safe(ab$sex_cat)),
  region     = factor(to_chr_safe(ab$region)),
  urban      = factor(to_chr_safe(ab$urban))
) |>
  dplyr::mutate(
    tested = !is.na(fls_all) & (fls_any_num == 1 | fls_any_num == TRUE)
  ) |>
  dplyr::filter(
    !is.na(cluster_id),
    !is.na(strata),
    !is.na(weight), weight > 0
  ) |>
  dplyr::select(-fls_any_num)

# ---- Survey design ----
design_all <- survey::svydesign(
  ids = ~cluster_id,
  strata = ~strata,
  weights = ~weight,
  data = df,
  nest = TRUE
)

design_tested <- subset(design_all, tested)

# ---- National means (tested) ----
nat <- lapply(c("fls_all", "fls_lit", "fls_num"), function(y) {
  est <- survey::svymean(stats::as.formula(paste0("~", y)), design_tested, na.rm = TRUE)
  ci  <- stats::confint(est)
  
  data.frame(
    outcome = y,
    mean = stats::coef(est)[1],
    se = survey::SE(est)[1],
    ci_low = ci[1],
    ci_high = ci[2]
  )
}) |>
  dplyr::bind_rows()

if (VERBOSE) print(nat)

# ---- Safe subgroup estimation (avoids unstable svyby()) ----
profile_one <- function(des, outcome, byvar) {
  v <- des$variables[[byvar]]
  if (is.null(v)) stop("byvar not found in design: ", byvar, call. = FALSE)
  
  lvls <- sort(unique(as.character(v)))
  lvls <- lvls[!is.na(lvls) & lvls != "NA"]
  
  rows <- lapply(lvls, function(g) {
    keep <- as.character(des$variables[[byvar]]) == g
    dsg  <- subset(des, keep)
    n_unw <- nrow(dsg$variables)
    
    if (n_unw == 0) {
      return(dplyr::tibble(
        outcome = outcome,
        dimension = byvar,
        group = g,
        n_unweighted = 0L,
        mean = NA_real_,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_
      ))
    }
    
    fml <- stats::as.formula(paste0("~", outcome))
    
    est <- try(survey::svymean(fml, dsg, na.rm = TRUE), silent = TRUE)
    if (inherits(est, "try-error")) {
      return(dplyr::tibble(
        outcome = outcome,
        dimension = byvar,
        group = g,
        n_unweighted = n_unw,
        mean = NA_real_,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_
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
      outcome = outcome,
      dimension = byvar,
      group = g,
      n_unweighted = n_unw,
      mean = m,
      se = se,
      ci_low = ci_low,
      ci_high = ci_high
    )
  })
  
  dplyr::bind_rows(rows)
}

outcomes <- c("fls_all", "fls_lit", "fls_num")
dims <- c("wealth_q5", "sex_cat", "region", "urban")

res <- dplyr::bind_rows(lapply(outcomes, function(y) {
  dplyr::bind_rows(lapply(dims, function(byv) profile_one(design_tested, y, byv)))
}))

# ---- Gap summary ----
gap_summary <- res |>
  dplyr::group_by(outcome, dimension) |>
  dplyr::summarise(
    min_mean = min(mean, na.rm = TRUE),
    max_mean = max(mean, na.rm = TRUE),
    gap = max_mean - min_mean,
    .groups = "drop"
  )

# ---- Save outputs ----
readr::write_csv(res, file.path(OUT_TABLES, "risk_profiles_fls.csv"))
readr::write_csv(gap_summary, file.path(OUT_TABLES, "risk_gaps_summary.csv"))

openxlsx::write.xlsx(
  res,
  file.path(OUT_TABLES, "risk_profiles_fls.xlsx"),
  overwrite = TRUE
)

openxlsx::write.xlsx(
  gap_summary,
  file.path(OUT_TABLES, "risk_gaps_summary.xlsx"),
  overwrite = TRUE
)

message("04_risk_profiles.R completed.")
