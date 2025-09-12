# ============================================================
# File:   03_Descriptive_Analysis.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Descriptive analyses of the social media survey dataset
#   - Frequencies, tables, plots for labelled variables
# ============================================================

# ------------------------------
# Load dependencies
# ------------------------------


# Setup ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
rm(list = ls())
gc()
pacman::p_load(tidyverse)

# Load helper functions (to_fac, to_num)
source("02_Scripts/Helpers.R")  # Load functions

# Load cleaned data --------------------------------------------------------
clean_data <- readRDS("01_Data/social_media_2025_clean_renamed.rds")


