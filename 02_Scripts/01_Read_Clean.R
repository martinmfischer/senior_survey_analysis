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
# rename variables
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
    info_politics_society       = f2_1_1,
    info_lifestyle              = f2_2_1,
    info_specific_needs         = f2_3_1,
    info_problem_solving        = f2_4_1,
    info_personal_interests     = f2_5_1,
    info_hobbies                = f2_6_1,
    info_close_social_circle    = f2_7_1,
    info_extended_social_circle = f2_8_1,
    
    # Perceptions of social media
    sm_suggestions_useful       = f3_1_1,
    sm_targeted_content_annoying = f3_2_1,
    sm_nonchronological_feed_useful = f3_3_1,
    sm_repeated_topics_annoying = f3_4_1,
    sm_agree_completely         = f3_99_1,
    sm_customize_useful         = f4_1_1,
    sm_block_accounts_useful    = f4_2_1,
    sm_follow_accounts_useful   = f4_3_1,
    sm_repeated_from_followed_annoying = f4_4_1,
    sm_discover_new_info_useful = f5_1_1,
    sm_unplanned_info_annoying  = f5_2_1,
    sm_unsearched_info_useful   = f5_3_1,
    sm_shared_info_useful       = f5_4_1,
    sm_lack_of_control          = f5_5_1,
    sm_likes_comments_helpful   = f6_1_1,
    sm_comments_annoying        = f6_2_1,
    sm_friend_interactions_useful = f6_3_1,
    sm_comments_enriching       = f6_4_1,
    sm_friends_likes_comments_annoying = f6_5_1,
    sm_disagree_completely      = f6_99_1,
    
    # Usage patterns
    sm_read_superficial         = f7_1_1,
    sm_use_for_info             = f7_2_1,
    sm_browse_while_waiting     = f7_3_1,
    sm_read_completely          = f7_4_1,
    sm_scroll_quickly           = f7_5_1,
    sm_consider_reading         = f7_6_1,
    sm_click_links_often        = f7_7_1,
    sm_short_sessions           = f7_8_1,
    
    # Interactions
    sm_like_post                = f8_1_1,
    sm_comment_post             = f8_2_1,
    sm_share_post               = f8_3_1,
    sm_forward_post_privately   = f8_4_1,
    
    # Control settings
    sm_block_user               = f9_1,
    sm_follow_user              = f9_2,
    sm_connect_request          = f9_3,
    sm_change_settings_more     = f9_4,
    sm_change_settings_less     = f9_5,
    sm_interact_for_more        = f9_6,
    sm_none_of_the_above        = f9_90,
    
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

# Prüfen
glimpse(data_renamed)


# ------------------------------
# Save cleaned data
# ------------------------------
saveRDS(clean_data, file = file.path("01_Data", "social_media_2025_clean.rds"))
saveRDS(data_renamed, file = file.path("01_Data", "social_media_2025_clean_renamed.rds"))

#label_table <- lapply(clean_data, function(x) attr(x, "labels"))

# End of script
