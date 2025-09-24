# ============================================================
# Project: LMU Social Media 2025
# File:   01_read_clean_data.R
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Load raw data file (.sav) from 0_Data/
#   - Perform basic cleaning (column names, NAs, empty rows/cols)
#   - Save cleaned dataset for downstream analysis
#
# ============================================================


rm(list = ls())
gc()


# ------------------------------
# Load required packages
# ------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  haven,      # read .sav (SPSS)
  dplyr,      # data manipulation
  janitor,    # clean column names
  stringr     # string handling
)

# ------------------------------
# File paths
# ------------------------------
raw_file <- file.path("01_Data", "25-041246_LMU_Social Media_2025.sav")

# ------------------------------
# Load data
# ------------------------------
raw_data <- haven::read_sav(raw_file)

# ------------------------------
# Basic cleaning
# ------------------------------
clean_data <- raw_data %>%
  # clean variable names: lower_snake_case
  janitor::clean_names() %>%
  # drop completely empty rows
  dplyr::filter(rowSums(is.na(.)) < ncol(.)) %>%
  # remove empty columns
  dplyr::select(where(~ !all(is.na(.)))) %>%
  # example: trim whitespace in character vars
  dplyr::mutate(across(where(is.character), str_trim))


library(dplyr)

# ------------------------------
# Rename variables
# ------------------------------
data_renamed <- clean_data %>%
  # Rename variables for clarity and consistency
  rename(
    # IDs & Screener
    participant_id              = i_tid,
    interview_duration_sec      = i_time,
    age_years                   = scr1a1,
    
    # Platform usage
    facebook_usage              = f1_1_1,
    instagram_usage             = f1_2_1,
    x_usage                     = f1_3_1,
    tiktok_usage                = f1_4_1,
    youtube_usage               = f1_5_1,
    platform_helper             = hv_auswahl_plattform,
    
    # Information use
    undirected_news             = f2_1_1,
    undirected_life             = f2_2_1,
    problem_specific_need       = f2_3_1,
    problem_solving             = f2_4_1,
    topic_interests             = f2_5_1,
    topic_hobbies               = f2_6_1,
    group_close                 = f2_7_1,
    group_extended              = f2_8_1,
    
    # Perceptions of social media
    implicit_1                  = f3_1_1,
    implicit_2                  = f3_2_1,
    implicit_3                  = f3_3_1,
    implicit_4                  = f3_4_1,
    attention_check_1           = f3_99_1,
    explicit_1                  = f4_1_1,
    explicit_2                  = f4_2_1,
    explicit_3                  = f4_3_1,
    explicit_4                  = f4_4_1,
    incidentalness_1            = f5_1_1,
    incidentalness_2            = f5_2_1,
    incidentalness_3            = f5_3_1,
    incidentalness_4            = f5_4_1,
    incidentalness_5            = f5_5_1,
    sociality_1                 = f6_1_1,
    sociality_2                 = f6_2_1,
    sociality_3                 = f6_3_1,
    sociality_4                 = f6_4_1,
    sociality_5                 = f6_5_1,
    attention_check_2           = f6_99_1,
    
    # Practices on social media
    snacking_1                  = f7_1_1,
    snacking_2                  = f7_2_1,
    snacking_3                  = f7_3_1,
    snacking_4                  = f7_4_1,
    snacking_5                  = f7_5_1,
    snacking_6                  = f7_6_1,
    snacking_7                  = f7_7_1,
    snacking_8                  = f7_8_1,
    engagement_1                = f8_1_1,
    engagement_2                = f8_2_1,
    engagement_3                = f8_3_1,
    engagement_4                = f8_4_1,
    curation_1                  = f9_1,
    curation_2                  = f9_2,
    curation_3                  = f9_3,
    curation_4                  = f9_4,
    curation_5                  = f9_5,
    curation_6                  = f9_6,
    curation_7                  = f9_90,
    
    # Socio-demographics
    gender                      = p_geschlecht,
    federal_state               = p_bundesland,
    main_occupation             = p_hauptbeschftigung,
    employment_status           = p_erwerbsttigkeit,
    occupational_group          = p_berufsgruppe,
    school_degree               = p_schulabschluss,
    vocational_degree           = p_berufsabschluss,
    municipality_size           = p_ortsgre,
    housing_type                = p_immobilienart,
    household_size              = p_haushalt_2025,
    children_in_household       = p_kinder_2025,
    empty_nest                  = p_empty_nest,
    marital_status              = p_familienstand,
    household_income            = p_haushaltsnettoeinkommen
  )

# Check
glimpse(data_renamed)


# ------------------------------
# Save cleaned data
# ------------------------------
saveRDS(clean_data, file = file.path("01_Data", "social_media_2025_clean.rds"))
saveRDS(data_renamed, file = file.path("01_Data", "social_media_2025_clean_renamed.rds"))

#label_table <- lapply(clean_data, function(x) attr(x, "labels"))

# End of script
