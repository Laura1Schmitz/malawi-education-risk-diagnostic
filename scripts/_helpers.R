# ------------------------------------------------------------
# _helpers.R — Small helper functions used across scripts
# ------------------------------------------------------------

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

pick_var <- function(df, candidates, required = TRUE, label = "var") {
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) >= 1) return(hit[[1]])
  if (required) {
    stop(sprintf("Required %s not found. Tried: %s", label, paste(candidates, collapse = ", ")),
         call. = FALSE)
  }
  NA_character_
}
