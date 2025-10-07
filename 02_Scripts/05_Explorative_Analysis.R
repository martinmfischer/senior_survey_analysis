# ============================================================
# File:   05_Explorative_Analysis.R
# Project: Social Media as an Information Source for Older Adults
# Author: Luise Anter
# Date:   2025-10-07
# Purpose:
#   - explorative analyses for all RQ
# ============================================================

# --- Setup -----------------------------------------------------------------

rm(list = ls(all = TRUE))
if(!require(pacman)) install.packages("pacman")
pacman::p_unload("all")

pacman::p_load(tidyverse, tidycomm, dplyr)

# --- Data ------------------------------------------------------------------

df <- readRDS("01_Data/social_media_2025_scored.rds")

# --- Preparation: Restrict analyses to Facebook, Instagram & YouTube ------

df <- df %>%
  mutate(platform_helper_short = case_when(
    platform_helper == 1 ~ "facebook",   
    platform_helper == 2 ~ "instagram", 
    platform_helper == 5 ~ "youtube",
    TRUE ~ NA_character_                         
  )) %>% 
  filter(!is.na(platform_helper_short))

# --- RQ1: Platform & user differences of usefulness perceptions -----------

df %>%
  dplyr::group_by(platform_helper_short) %>%
  describe(idx_implicit, idx_explicit, idx_sociality, idx_incidentalness)

anovas_perceptions_by_platform <- df %>% 
  unianova(platform_helper_short, idx_implicit, idx_explicit, idx_sociality, 
           idx_incidentalness, descriptives = TRUE, post_hoc = TRUE)

View(anovas_perceptions_by_platform)

df %>% 
  dplyr::group_by(education_cat) %>%
  describe(idx_implicit, idx_explicit, idx_sociality, idx_incidentalness)

anovas_perceptions_by_platform <- df %>% 
  unianova(education_cat, idx_implicit, idx_explicit, idx_sociality, 
           idx_incidentalness, descriptives = TRUE, post_hoc = TRUE)

View(anovas_perceptions_by_platform)
  

# --- RQ2: Platform & user differences of practices ------------------------

df %>%
  dplyr::group_by(platform_helper_short) %>%
  describe(idx_engagement, idx_snacking, idx_curation)

anovas_practices_by_platform <- df %>% 
  unianova(platform_helper_short, idx_engagement, idx_snacking, 
           idx_curation, descriptives = TRUE, post_hoc = TRUE)

View(anovas_practices_by_platform)
