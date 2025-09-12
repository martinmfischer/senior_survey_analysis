# ============================================================
# File:   04_Regression.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Regression analyses of social media survey dataset
#   - Example: linear model predicting a social media attitude variable
# ============================================================

# ------------------------------
# Load dependencies
# ------------------------------
rm(list = ls())
gc()

pacman::p_load(tidyverse)

# Source helpers (to_num / to_fac)
source("02_Scripts/Helpers.R")  # Load functions

# Load scored data (created by exploratory script)
clean_data <- readRDS("01_Data/social_media_2025_scored.rds")


