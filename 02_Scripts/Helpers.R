# ============================================================
# File:   helpers.R
# Project:  Social Media as an Information Source for Older Adults
# Author: Martin Fischer
# Date:   2025-10-02
# Purpose:
#   - Various helper functions
# ============================================================

# --- Setup -----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr, ggplot2)

# --- Coverters -------------------------------------------------------------

# to_num: convert haven::labelled / factor to numeric (for models
to_num <- function(x) {
  if (haven::is.labelled(x)) {
    as.numeric(x)
  } else {
    x
  }
}

# to_fac: convert haven::labelled to ordered factor (for labelled plots)
to_fac <- function(x) {
  if (haven::is.labelled(x)) {
    haven::as_factor(x, levels = "labels", ordered = TRUE)
  } else {
    x
  }
}

## Usage
# clean_data %>% ggplot(aes(x = to_fac(explicit_1))) + geom_bar()
# Optional ggplot helper: use labelled variable for x-axis
# Example: ggplot(data, aes(x = to_fac(f4_1_1))) + geom_bar()
# lm(to_num(explicit_1) ~ age + gender, data = clean_data)

# --- Reliability helpers ---------------------------------------------------

# omega_h: # omega_h: compute hierarchical omega for a set of items (>= 3 items),
# drop other returns from psych::omega()
omega_h <- function(df, items) {
  mat <- df |> dplyr::select(all_of(items)) |> mutate(across(everything(), to_num)) |> as.matrix()
  if (ncol(mat) < 3) return(NA_real_)  # ωh nur sinnvoll ab 3 Items
  suppressWarnings({
    out <- try(psych::omega(mat, nfactors = 1, plot = FALSE), silent = TRUE)
  })
  if (inherits(out, "try-error")) return(NA_real_)
  as.numeric(out$omega_h)
}

## Usage example
# omega_h:
# omega_h(data, items)

# choose_items_by_omega: if omega_h < thr, try drop-one; otherwise keep all
choose_items_by_omega <- function(df, items, thr = 0.7) {
  if (length(items) <= 2) {
    return(list(
      items_used = items,
      omega_h = NA_real_,
      dropped = NA_character_,
      note = "Omega-h not feasible for <= 2 items"
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

##Usage example
# choice <- choose_items_by_omega(data_rev, items_used, thr = 0.7)
# choice$items_used   # final items after optional drop
# choice$omega_h      # resulting omega_h

# spearman_brown_2items: reliability for a 2-item scale
spearman_brown_2items <- function(x, y) {
  x <- to_num(x); y <- to_num(y)
  r <- suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
  if (is.na(r)) return(NA_real_)
  (2 * r) / (1 + r)
}

## Usage example
# sb <- spearman_brown_2items(data_rev[[items[1]]], data_rev[[items[2]]])
# if (!is.na(sb) && sb >= 0.7) {
#   idx_2item <- row_mean(data_rev, items, min_prop = 1.0)  # require both items
# }

# --- Model requirements report ---------------------------------------------

# check_requirements_md: quick diagnostics for an lm() model
# Returns a tibble for Markdown reporting
check_requirements_md <- function(lm_model){
  data <- lm_model$model %>% mutate(across(where(is.factor), as.numeric))
  n_obs <- nrow(data)
  
  # Shapiro-Wilk on residuals (only informative for small/moderate n)
  if(n_obs < 3){
    shapiro_p <- NA
    shapiro_result <- "Too few observations for Shapiro–Wilk test"
  } else if(n_obs > 500){
    shapiro_p <- NA
    shapiro_result <- "N>500: Shapiro–Wilk is not informative; inspect Q–Q plot instead"
  } else {
    shapiro_p <- shapiro.test(residuals(lm_model))$p.value
    shapiro_result <- ifelse(shapiro_p > 0.05,
                             "Residual normality not rejected (GOOD)",
                             "Residual normality rejected (BAD)")
  }
  
  # VIF (Variance Inflation Factor)
  vif_values <- vif(lm_model)
  vif_result <- ifelse(any(vif_values > 10),
                       "Multicollinearity detected (max VIF > 10) (BAD)",
                       "No severe multicollinearity (max VIF <= 10) (GOOD)")
  
  # Breusch-Pagan
  bp_p <- bptest(lm_model)$p.value
  bp_result <- ifelse(bp_p > 0.05,
                      "Homoscedasticity not detected (GOOD)",
                      "Heteroscedasticity detected (BAD)")
  
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

## Usage example

# --- End of script ---------------------------------------------------------
