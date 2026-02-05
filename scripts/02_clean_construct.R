#### ------------------------------------------------------------
#### 02_clean_construct.R — Clean and construct FLS outcomes (fs spine)
#### ------------------------------------------------------------

rm(list = ls())
gc()

source(here::here("scripts", "00_setup.R"))

fs <- readRDS(here::here("data", "interim", "fs_raw_clean_names.rds"))

vec_data_safe <- function(x) {
  if (inherits(x, "haven_labelled")) return(unclass(x))
  x
}

to_num_safe <- function(x) {
  v <- vec_data_safe(x)
  suppressWarnings(as.numeric(v))
}

to_chr_safe <- function(x) {
  v <- vec_data_safe(x)
  as.character(v)
}

standardize_missing_num <- function(x) {
  v <- to_num_safe(x)
  v[v %in% c(7, 8, 9, 97, 98, 99, 999, 9999)] <- NA_real_
  v
}

standardize_missing_chr <- function(x) {
  v <- to_chr_safe(x)
  v[v %in% c("", " ", "?", "NA", "N/A")] <- NA_character_
  v
}

is_correct_item <- function(x) {
  labs <- attr(x, "labels")
  
  if (!is.null(labs) && length(labs) > 0) {
    nms <- names(labs)
    if (!is.null(nms)) {
      nms_u <- toupper(nms)
      idx <- which(grepl("CORRECT|RIGHT|YES", nms_u) & !grepl("NOT", nms_u))
      if (length(idx) >= 1) {
        correct_code <- unname(labs[[idx[[1]]]])
        
        if (is.character(correct_code)) {
          v <- standardize_missing_chr(x)
          return(as.numeric(v == as.character(correct_code)))
        }
        
        v <- standardize_missing_num(x)
        return(as.numeric(v == as.numeric(correct_code)))
      }
    }
  }
  
  v_num <- standardize_missing_num(x)
  if (all(is.na(v_num))) {
    return(rep(NA_real_, length(x)))
  }
  
  out <- rep(NA_real_, length(v_num))
  out[v_num == 1] <- 1
  out[v_num == 2] <- 0
  out
}

row_mean_safe <- function(df) {
  if (ncol(df) == 0) return(rep(NA_real_, nrow(df)))
  m <- as.matrix(df)
  if (ncol(m) == 1) return(as.numeric(m[, 1]))
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

row_any_nonmiss <- function(df) {
  if (ncol(df) == 0) return(rep(FALSE, nrow(df)))
  m <- as.matrix(df)
  apply(m, 1, function(r) any(!is.na(r)))
}

fs_vars <- names(fs)
fl_vars <- fs_vars[grepl("^fl", fs_vars, ignore.case = TRUE)]
if (length(fl_vars) == 0) stop("No FLS variables found in fs (expected variables starting with 'fl').", call. = FALSE)

id_vars <- c("hh1", "hh2", "ln")
missing_ids <- setdiff(id_vars, names(fs))
if (length(missing_ids) > 0) stop(paste0("Missing required identifier(s) in fs: ", paste(missing_ids, collapse = ", ")), call. = FALSE)

fs_items <- fs[, fl_vars, drop = FALSE]

item_correct <- lapply(fs_items, is_correct_item)
item_correct <- as.data.frame(item_correct)

literacy_items <- names(item_correct)[grepl("^flb|^fl2(2|3)", names(item_correct), ignore.case = TRUE)]
numeracy_items <- names(item_correct)[grepl("^fl2(4|5|6)", names(item_correct), ignore.case = TRUE)]

lit_mat <- item_correct[, literacy_items, drop = FALSE]
num_mat <- item_correct[, numeracy_items, drop = FALSE]

fs_clean <- dplyr::tibble(
  cluster_id  = to_num_safe(fs$hh1),
  hh_id       = to_num_safe(fs$hh2),
  child_line  = to_num_safe(fs$ln),
  fls_any     = row_any_nonmiss(item_correct),
  fls_n_items = rowSums(!is.na(as.matrix(item_correct))),
  fls_lit     = row_mean_safe(lit_mat),
  fls_num     = row_mean_safe(num_mat),
  fls_all     = row_mean_safe(item_correct)
)

fs_clean <- dplyr::bind_cols(fs_clean, item_correct)

fs_clean <- fs_clean %>%
  dplyr::filter(!is.na(cluster_id), !is.na(hh_id), !is.na(child_line))

summary(fs_clean$fls_lit)
summary(fs_clean$fls_num)
summary(fs_clean$fls_all)

dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(fs_clean, here::here("data", "processed", "fs_clean.rds"))

dir.create(here::here("metadata"), recursive = TRUE, showWarnings = FALSE)

meta <- dplyr::tibble(
  n_raw = nrow(fs),
  n_clean = nrow(fs_clean),
  share_with_any_fls = mean(fs_clean$fls_any, na.rm = TRUE),
  avg_n_items_nonmiss = mean(fs_clean$fls_n_items, na.rm = TRUE),
  n_lit_items = length(literacy_items),
  n_num_items = length(numeracy_items),
  mean_fls_lit = mean(fs_clean$fls_lit, na.rm = TRUE),
  mean_fls_num = mean(fs_clean$fls_num, na.rm = TRUE),
  mean_fls_all = mean(fs_clean$fls_all, na.rm = TRUE)
)

readr::write_csv(meta, here::here("metadata", "fs_clean_summary.csv"))
