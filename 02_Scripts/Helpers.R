# ============================================================
# File:   helpers.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Various Helper Functions
# ============================================================

# ------------------------------
# Load dependencies
# ------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr, ggplot2)

# ------------------------------
# Functions
# ------------------------------

# Convert labelled variable to numeric (for models)
to_num <- function(x) {
  if (haven::is.labelled(x)) {
    as.numeric(x)
  } else {
    x
  }
}

# Convert labelled variable to ordered factor (for plots)
to_fac <- function(x) {
  if (haven::is.labelled(x)) {
    haven::as_factor(x, levels = "labels", ordered = TRUE)
  } else {
    x
  }
}

# Reverse code 5-point Likert scales (adds *_rev columns for the given items)
# 1 -> 5, 2 -> 4, 3 -> 3, 4 -> 2, 5 -> 1
reverse_likert <- function(x) {
  xn <- to_num(x)
  ifelse(is.na(xn), NA, 6 - xn)
}

# Index building: row mean index with missing tolerance
# min_prop = minimum proportion answered, default = 0.6
row_mean <- function(df, items, min_prop = 0.6) {
  mat <- df |> dplyr::select(all_of(items)) |> as.data.frame()
  mat[] <- lapply(mat, to_num)
  n_items <- ncol(mat)
  ok <- rowSums(!is.na(mat)) >= ceiling(min_prop * n_items)
  idx <- rowMeans(mat, na.rm = TRUE)
  idx[!ok] <- NA_real_
  idx
}

# Extract omega_h (hierarchical omega)
# Drop other returns from psych::omega()
omega_h <- function(df, items) {
  mat <- df |> dplyr::select(all_of(items)) |> mutate(across(everything(), to_num)) |> as.matrix()
  if (ncol(mat) < 3) return(NA_real_)  # ωh nur sinnvoll ab 3 Items
  suppressWarnings({
    out <- try(psych::omega(mat, nfactors = 1, plot = FALSE), silent = TRUE)
  })
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out$omega_h)
}

# Drop-one rule: if ωh < thr (o.7), test scale without each item 
choose_items_by_omega <- function(df, items, thr = 0.7) {
  if (length(items) <= 2) {
    return(list(
      items_used = items,
      omega_h = NA_real_,
      dropped = NA_character_,
      note = "Omega-h nicht berechenbar bei <= 2 Items"
    ))
  }
  
  
  base <- omega_h(df, items)
  if (!is.na(base) && base >= thr) {
    return(list(items_used = items, omega_h = base, dropped = NA_character_))
  }
  # Test: drop one item at a time
  candidates <- purrr::map(items, ~ setdiff(items, .x))
  omegas <- purrr::map_dbl(candidates, ~ omega_h(df, .x))
  best_i <- which.max(omegas)
  best_omega <- omegas[best_i]
  if (!is.na(best_omega) && best_omega >= thr) {
    return(list(
      items_used = candidates[[best_i]],
      omega_h = best_omega,
      dropped = setdiff(items, candidates[[best_i]])[1]
    ))
  } else {
    return(list(items_used = items, omega_h = base, dropped = NA_character_))
  }
}

# Spearman-Brown reliability for 2-item scales
spearman_brown_2items <- function(x, y) {
  x <- to_num(x); y <- to_num(y)
  r <- suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
  if (is.na(r)) return(NA_real_)
  (2 * r) / (1 + r)
}

# Item list adjustment (reverse suffix)
# Add *_rev to item names if they are in rev_items
apply_rev_suffix <- function(items, rev_items) {
  ifelse(items %in% rev_items, paste0(items, "_rev"), items)
}

# Optional ggplot helper: use labelled variable for x-axis
# Example: ggplot(data, aes(x = to_fac(f4_1_1))) + geom_bar()


check_requirements_md <- function(lm_model){
  data <- lm_model$model %>% mutate(across(where(is.factor), as.numeric))
  
  shapiro_p <- shapiro.test(residuals(lm_model))$p.value
  shapiro_result <- ifelse(shapiro_p > 0.05,
                           "Normalverteilung nicht verletzt (gut)",
                           "Normalverteilung verletzt (SCHLECHT)")
  
  vif_values <- vif(lm_model)
  vif_result <- ifelse(any(vif_values > 10),
                       "Multikollinearität vorhanden (SCHLECHT)",
                       "Keine Multikollinearität (gut)")
  
  bp_p <- bptest(lm_model)$p.value
  bp_result <- ifelse(bp_p > 0.05,
                      "Keine Heteroskedastizität (gut)",
                      "Heteroskedastizität vorhanden (SCHLECHT)")
  
  tibble(
    DV = NA_character_,
    Block = NA_character_,
    Model_Call = deparse(lm_model$call),
    Shapiro_Wilk_p = round(shapiro_p, 3),
    Shapiro_Wilk_Result = shapiro_result,
    Max_VIF = round(max(vif_values), 3),
    VIF_Result = vif_result,
    Breusch_Pagan_p = round(bp_p, 3),
    BP_Result = bp_result
  )
}

# ------------------------------
# Usage:
# ------------------------------
# source("R/helpers.R")  # Load functions

# clean_data %>% ggplot(aes(x = to_fac(explicit_1))) + geom_bar()

# lm(to_num(explicit_1) ~ age + gender, data = clean_data)

# reverse_likert: 
# data_rev <- clean_data %>%
# dplyr::mutate(across(all_of(rev_items), ~ reverse_likert(.x), .names = "{.col}_rev"))

# apply_rev_suffix:
# items_used <- apply_rev_suffix(items, rev_items)

# omega_h:
# omega_h(data_rev, items_used)

# choose_items_by_omega:
# choice <- choose_items_by_omega(data_rev, items_used, thr = 0.7)
# choice$items_used   # final items after optional drop
# choice$omega_h      # resulting omega_h

# row_mean:
# idx <- row_mean(data_rev, choice$items_used, min_prop = 0.6)

# spearman_brown_2items:
# sb <- spearman_brown_2items(data_rev[[items[1]]], data_rev[[items[2]]])
# if (!is.na(sb) && sb >= 0.7) {
#   idx_2item <- row_mean(data_rev, items, min_prop = 1.0)  # require both items
# }
