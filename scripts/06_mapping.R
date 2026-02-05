# ------------------------------------------------------------
# 06_mapping.R — District maps: learning vulnerability + access/progression
# ------------------------------------------------------------

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "_helpers.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(survey)
  library(sf)
  library(ggplot2)
  library(janitor)
  library(openxlsx)
  library(scales)
})

# ---- Helpers ----
pct_limits <- function(x, lo = 0.05, hi = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- stats::quantile(x, probs = c(lo, hi), na.rm = TRUE, names = FALSE)
  qs[1] <- max(0, qs[1])
  qs[2] <- min(1, qs[2])
  if (!is.finite(qs[1]) || !is.finite(qs[2]) || qs[1] >= qs[2]) return(c(0, 1))
  qs
}

# ---- Inputs ----
ab <- readRDS(file.path(root, "data", "processed", "analysis_base.rds"))

boundary_shp <- file.path(
  paths$mics_gis,
  "Survey Boundaries",
  "MalawiMICS2019-20Boundaries_1",
  "Shapefiles",
  "mics_boundaries3.shp"
)

xwalk_shp <- file.path(
  paths$mics_gis,
  "GPS Datasets",
  "MalawiMICS2019-20GPS",
  "MalawiMICS2019-20GPS.shp"
)

if (!file.exists(boundary_shp)) {
  stop("Boundary shapefile not found:\n", boundary_shp, call. = FALSE)
}
if (!file.exists(xwalk_shp)) {
  stop("Cluster GPS shapefile not found:\n", xwalk_shp, call. = FALSE)
}

# ---- Read GIS ----
bnd <- sf::st_read(boundary_shp, quiet = TRUE) |>
  sf::st_make_valid() |>
  janitor::clean_names()

xwalk <- sf::st_read(xwalk_shp, quiet = TRUE) |>
  as.data.frame() |>
  janitor::clean_names()

# ---- Join fields (MICS schema) ----
cluster_v <- "hh1"
district_code_v <- "geocodes"
district_name_v <- "geonames"

bnd_district_code_v <- "geocodes"
bnd_district_name_v <- "geonames"

xwalk2 <- xwalk |>
  dplyr::transmute(
    cluster_id = to_num_safe(.data[[cluster_v]]),
    district_code = to_chr_safe(.data[[district_code_v]]),
    district_name = to_chr_safe(.data[[district_name_v]]),
    join_key = stringr::str_trim(district_code)
  ) |>
  dplyr::filter(!is.na(cluster_id), !is.na(join_key), join_key != "")

bnd2 <- bnd |>
  dplyr::mutate(
    district_code = to_chr_safe(.data[[bnd_district_code_v]]),
    district_name = to_chr_safe(.data[[bnd_district_name_v]]),
    join_key = stringr::str_trim(district_code)
  ) |>
  dplyr::filter(!is.na(join_key), join_key != "")

district_labels <- bnd2 |>
  sf::st_drop_geometry() |>
  dplyr::distinct(join_key, district_name)

# ---- Prepare analysis data (attach district) ----
req <- c(
  "cluster_id", "strata", "weight",
  "fls_all", "fls_lit", "fls_num", "fls_any",
  "school_att", "age_years", "grade_cur"
)
miss <- setdiff(req, names(ab))
if (length(miss) > 0) {
  stop("Missing in analysis_base: ", paste(miss, collapse = ", "), call. = FALSE)
}

df0 <- ab |>
  dplyr::transmute(
    cluster_id = to_num_safe(cluster_id),
    strata     = to_num_safe(strata),
    weight     = to_num_safe(weight),
    
    fls_any = to_num_safe(fls_any),
    fls_all = to_num_safe(fls_all),
    fls_lit = to_num_safe(fls_lit),
    fls_num = to_num_safe(fls_num),
    
    school_att_raw = to_num_safe(school_att),
    age_years  = to_num_safe(age_years),
    grade_cur  = to_num_safe(grade_cur)
  ) |>
  dplyr::mutate(
    tested = (fls_any == 1) & !is.na(fls_all),
    
    school_att = dplyr::case_when(
      school_att_raw == 1 ~ 1,
      school_att_raw %in% c(0, 2) ~ 0,
      TRUE ~ NA_real_
    ),
    
    over_age_universe = (school_att == 1) & !is.na(age_years) & !is.na(grade_cur),
    over_age = dplyr::if_else(
      over_age_universe,
      as.numeric(age_years >= (grade_cur + 6)),
      NA_real_
    )
  ) |>
  dplyr::filter(
    !is.na(cluster_id),
    !is.na(strata),
    !is.na(weight), weight > 0
  ) |>
  dplyr::left_join(xwalk2, by = "cluster_id") |>
  dplyr::filter(!is.na(join_key), join_key != "")

# ---- Core estimator ----
estimate_district <- function(data, y_var) {
  des <- survey::svydesign(
    ids = ~cluster_id,
    strata = ~strata,
    weights = ~weight,
    data = data,
    nest = TRUE
  )
  
  est <- survey::svyby(
    stats::as.formula(paste0("~", y_var)),
    ~join_key,
    des,
    survey::svymean,
    na.rm = TRUE,
    keep.names = FALSE
  )
  
  est <- as.data.frame(est) |>
    dplyr::rename(
      join_key = join_key,
      mean = !!y_var,
      se = se
    ) |>
    dplyr::mutate(
      ci_low  = pmax(0, mean - 1.96 * se),
      ci_high = pmin(1, mean + 1.96 * se)
    )
  
  n_unw <- data |>
    dplyr::count(join_key, name = "n_unweighted")
  
  est |>
    dplyr::left_join(n_unw, by = "join_key") |>
    dplyr::left_join(district_labels, by = "join_key") |>
    dplyr::relocate(district_name, .after = join_key)
}

plot_map <- function(bnd_map, fill_var, title, subtitle, outfile_stub, lo = 0.05, hi = 0.95) {
  lims <- pct_limits(bnd_map[[fill_var]], lo = lo, hi = hi)
  
  p <- ggplot(bnd_map) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", linewidth = 0.2) +
    scale_fill_gradient(
      name = NULL,
      limits = lims,
      oob = scales::squish,
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Source: UNICEF MICS6 Malawi 2019–2020. Estimates account for sampling weights, clustering, and stratification."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(OUT_MAPS, paste0(outfile_stub, ".png")), p, width = 10, height = 7, dpi = 300)
  ggsave(file.path(OUT_MAPS, paste0(outfile_stub, ".pdf")), p, width = 10, height = 7)
  
  invisible(p)
}

# ---- Indicators ----
df_learning <- df0 |>
  dplyr::filter(tested) |>
  dplyr::mutate(
    low_fls_all = 1 - fls_all,
    low_fls_lit = 1 - fls_lit,
    low_fls_num = 1 - fls_num
  )

df_access <- df0 |>
  dplyr::filter(!is.na(school_att)) |>
  dplyr::mutate(not_attending = 1 - school_att)

df_overage <- df0 |>
  dplyr::filter(over_age_universe)

# ---- Run: estimates + outputs + maps ----
run_one <- function(data, y_var, stub, title, subtitle) {
  est <- estimate_district(data, y_var)
  
  readr::write_csv(est, file.path(OUT_TABLES, paste0("map_district_", stub, ".csv")))
  openxlsx::write.xlsx(est, file.path(OUT_TABLES, paste0("map_district_", stub, ".xlsx")), overwrite = TRUE)
  
  bnd_map <- bnd2 |>
    dplyr::left_join(est, by = "join_key")
  
  plot_map(
    bnd_map = bnd_map,
    fill_var = "mean",
    title = title,
    subtitle = subtitle,
    outfile_stub = paste0("map_", stub, "_district"),
    lo = 0.05, hi = 0.95
  )
  
  invisible(est)
}

run_one(
  df_learning, "low_fls_all", "low_fls_all",
  "Learning vulnerability (low foundational learning), Malawi MICS 2019–2020",
  "District-level share not demonstrating overall foundational learning (tested children)"
)

run_one(
  df_learning, "low_fls_lit", "low_fls_lit",
  "Learning vulnerability (low literacy), Malawi MICS 2019–2020",
  "District-level share not demonstrating foundational literacy (tested children)"
)

run_one(
  df_learning, "low_fls_num", "low_fls_num",
  "Learning vulnerability (low numeracy), Malawi MICS 2019–2020",
  "District-level share not demonstrating foundational numeracy (tested children)"
)

run_one(
  df_access, "not_attending", "not_attending",
  "School exclusion (not attending), Malawi MICS 2019–2020",
  "District-level share not attending school (all children with non-missing attendance)"
)

run_one(
  df_overage, "over_age", "over_age",
  "Grade progression challenge (over-age), Malawi MICS 2019–2020",
  "District-level share over-age for grade (currently attending with non-missing age & grade)"
)

message("06_mapping.R completed.")
