# ------------------------------------------------------------
# 03b_validate_design.R — Validate survey design variables (weights, PSU, strata)
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

# ---- Inputs ----
ab <- readRDS(file.path(root, "data", "processed", "analysis_base.rds"))

req <- c("cluster_id", "strata", "weight", "fls_any", "fls_all", "fls_lit", "fls_num")
miss <- setdiff(req, names(ab))
if (length(miss) > 0) {
  stop("Missing required variables: ", paste(miss, collapse = ", "), call. = FALSE)
}

# ---- Analysis frame ----
ab_d <- ab |>
  dplyr::mutate(
    weight     = to_num_safe(weight),
    strata     = to_num_safe(strata),
    cluster_id = to_num_safe(cluster_id),
    
    fls_any_num = to_num_safe(fls_any),
    tested = (fls_any_num == 1 | fls_any_num == TRUE) & !is.na(fls_all)
  ) |>
  dplyr::filter(
    !is.na(weight), weight > 0,
    !is.na(strata),
    !is.na(cluster_id)
  ) |>
  dplyr::select(-fls_any_num)

n_all <- nrow(ab_d)
n_tested <- sum(ab_d$tested, na.rm = TRUE)

# ---- Survey design ----
design_all <- survey::svydesign(
  ids = ~cluster_id,
  strata = ~strata,
  weights = ~weight,
  data = ab_d,
  nest = TRUE
)

design_tested <- subset(design_all, tested)

# ---- Helpers: weighted and unweighted summaries ----
wmean <- function(des, var) {
  as.numeric(survey::svymean(stats::as.formula(paste0("~", var)), des, na.rm = TRUE))
}

wse <- function(des, var) {
  est <- survey::svymean(stats::as.formula(paste0("~", var)), des, na.rm = TRUE)
  as.numeric(survey::SE(est))
}

uw_mean <- function(df, var) mean(df[[var]], na.rm = TRUE)
uw_sd   <- function(df, var) stats::sd(df[[var]], na.rm = TRUE)

vars <- c("fls_all", "fls_lit", "fls_num")
tested_df <- dplyr::filter(ab_d, tested)

res_tested <- dplyr::tibble(
  sample = "tested",
  n = n_tested,
  n_all = n_all,
  share_tested = n_tested / n_all,
  var = vars,
  mean_unweighted = vapply(vars, function(v) uw_mean(tested_df, v), numeric(1)),
  sd_unweighted   = vapply(vars, function(v) uw_sd(tested_df, v), numeric(1)),
  mean_weighted   = vapply(vars, function(v) wmean(design_tested, v), numeric(1)),
  se_weighted     = vapply(vars, function(v) wse(design_tested, v), numeric(1))
)

res_all <- dplyr::tibble(
  sample = "all",
  n = n_all,
  n_all = n_all,
  share_tested = n_tested / n_all,
  var = vars,
  mean_unweighted = vapply(vars, function(v) uw_mean(ab_d, v), numeric(1)),
  sd_unweighted   = vapply(vars, function(v) uw_sd(ab_d, v), numeric(1)),
  mean_weighted   = vapply(vars, function(v) wmean(design_all, v), numeric(1)),
  se_weighted     = vapply(vars, function(v) wse(design_all, v), numeric(1))
)

out <- dplyr::bind_rows(res_tested, res_all) |>
  dplyr::mutate(
    ci_low = mean_weighted - 1.96 * se_weighted,
    ci_high = mean_weighted + 1.96 * se_weighted
  )

dir.create(file.path(root, "metadata"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, file.path(root, "metadata", "design_validation.csv"))

design_meta <- dplyr::tibble(
  n_all = n_all,
  n_tested = n_tested,
  share_tested = n_tested / n_all,
  n_psu = dplyr::n_distinct(ab_d$cluster_id),
  n_strata = dplyr::n_distinct(ab_d$strata),
  weight_min = min(ab_d$weight, na.rm = TRUE),
  weight_p01 = as.numeric(stats::quantile(ab_d$weight, 0.01, na.rm = TRUE)),
  weight_median = as.numeric(stats::quantile(ab_d$weight, 0.50, na.rm = TRUE)),
  weight_p99 = as.numeric(stats::quantile(ab_d$weight, 0.99, na.rm = TRUE)),
  weight_max = max(ab_d$weight, na.rm = TRUE)
)

readr::write_csv(design_meta, file.path(root, "metadata", "design_validation_meta.csv"))

if (VERBOSE) {
  print(out)
  print(design_meta)
}

message("03b_validate_design.R completed.")
