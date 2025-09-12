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

# Optional ggplot helper: use labelled variable for x-axis
# Example: ggplot(data, aes(x = to_fac(f4_1_1))) + geom_bar()



# ------------------------------
# Usage:
# ------------------------------
# source("R/helpers.R")  # Load functions
# clean_data %>% ggplot(aes(x = to_fac(f4_1_1))) + geom_bar()
# lm(to_num(f4_1_1) ~ age + gender, data = clean_data)
