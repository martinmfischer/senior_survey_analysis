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
pacman::p_load(tidyverse, tidycomm)

# Load helper functions (to_fac, to_num)
source("02_Scripts/Helpers.R")  # Load functions

# Load cleaned data --------------------------------------------------------
clean_data <- readRDS("01_Data/social_media_2025_clean_renamed.rds")

# ------------------------------
# Recode reverse-coded items
# ------------------------------

data_rev <- clean_data %>% reverse_scale(implicit_2,
                             implicit_4,
                             explicit_4,
                             incidentalness_2,
                             incidentalness_5,
                             sociality_2,
                             sociality_5,
                             snacking_2,
                             snacking_4,
                             snacking_6,
                             snacking_7,
                             ### select lower and upper bound for recode
                             lower_end = 5,
                             upper_end = 1)




# ------------------------------
# Compute reliability + mean indices
# ------------------------------

data_idx <- data_rev %>%
  # Perceived usefulness
  add_index(idx_implicit,
            implicit_1, implicit_2_rev, implicit_3, implicit_4_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_explicit,
            explicit_1, explicit_2, explicit_3, explicit_4_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_incidentalness,
            incidentalness_1, incidentalness_2_rev, incidentalness_3, incidentalness_4, incidentalness_5_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_sociality,
            sociality_1, sociality_2_rev, sociality_3, sociality_4, sociality_5_rev,
            cast.numeric = TRUE) %>%
  # Practices
  add_index(idx_snacking, 
            snacking_1, snacking_2_rev, snacking_3, snacking_4_rev,
            snacking_5, snacking_6_rev, snacking_7_rev, snacking_8,
            cast.numeric = TRUE) %>%
  add_index(idx_engagement, 
            engagement_1, engagement_2, engagement_3, engagement_4,
            cast.numeric = TRUE) %>%
  add_index(idx_curation, 
            curation_1, curation_2, curation_3, curation_4, curation_5, curation_6, curation_7,
            cast.numeric = TRUE) %>%
  # Information use (2-item indices)
  add_index(idx_info_undirected, undirected_news,       undirected_life,           cast.numeric = TRUE) %>%
  add_index(idx_info_topic,      topic_interests,       topic_hobbies,             cast.numeric = TRUE) %>%
  add_index(idx_info_problem,    problem_specific_need, problem_solving,           cast.numeric = TRUE) %>%
  add_index(idx_info_group,      group_close,           group_extended,            cast.numeric = TRUE)

data_idx %>% get_reliability(type = "hierarchical")
data_idx %>% get_reliability(idx_info_undirected, idx_info_topic,
                             idx_info_problem, idx_info_group, type = "alpha")

# ------------------------------
# Recode gender + education
# ------------------------------

# Recode education

data_idx <- data_idx %>%
  mutate(
    school_degree_num = to_num(school_degree),
    education_cat = recode_factor(school_degree_num,
                                  `1`  = "low",
                                  `90` = "low",
                                  `2`  = "medium",
                                  `3`  = "medium",
                                  `4`  = "medium",
                                  `5`  = "high",
                                  `6`  = "high",
                                  `91` = NA_character_,
                                  `97` = NA_character_
    ),
    education_cat = factor(education_cat, levels = c("low","medium","high"))
  )

# Recode gender

data_idx <- data_idx %>%
  mutate(
    gender_binary = recode_factor(gender,
                                  "weiblich"  = "female",
                                  "männlich" = "non-female"
    ),
    gender_binary = factor(gender_binary, levels = c("non-female","female"))
  )

# --------------------------------------------------------------
# Sociodemographic composition and characteristics of the sample
# --------------------------------------------------------------

# Age
data_idx %>% 
  describe(age_years)

# Gender
data_idx  %>% 
  tab_frequencies(gender_binary)

# Education
data_idx  %>% 
  tab_frequencies(education_cat)

data_idx  %>% 
  tab_frequencies(school_degree)

# Income
data_idx %>% 
  tab_frequencies(household_income)

# Social Media Use
data_idx %>% 
  tab_frequencies(platform_helper)

data_idx %>% 
  tab_frequencies(facebook_usage)

data_idx %>% 
  tab_frequencies(instagram_usage)

data_idx %>% 
  tab_frequencies(x_usage)

data_idx %>% 
  tab_frequencies(tiktok_usage)

data_idx %>% 
  tab_frequencies(youtube_usage)