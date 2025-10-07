# ============================================================
# File:   03_Descriptive_Analysis.R
# Project: Social Media as an Information Source for Older Adults
# Author: Martin Fischer
# Date:   2025-10-02
# Purpose:
#   - Build indices; reliability checks; recode variables
#   - Descriptive analyses (sample characteristics, RQ1, RQ2)
# ============================================================

# Setup ---------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
rm(list = ls())
gc()

pacman::p_load(tidyverse, psych, tidycomm, dplyr)

source("02_Scripts/Helpers.R")  # to_fac, to_num, omega_h, 
                                # choose_items_by_omega, spearman_brown_2items

# Load clean data -----------------------------------------------------------

clean_data <- readRDS("01_Data/social_media_2025_clean_renamed.rds")

# Reverse-code items (Likert 1–5) -------------------------------------------
# Note: reverse_scale creates *_rev columns; here 1<->5 mapping

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

# --- Recode curation-items -------------------------------------------------
# Note: Checkbox-Scale, 'selected' was coded inconsistently (1 - 6)

curation_opts <- paste0("curation_", 1:6)
data_rev <- data_rev %>%
  mutate(
    across(all_of(curation_opts),
           ~ dplyr::case_when(
             is.na(.)           ~ NA_integer_,   
             . == 0             ~ 0L,            
             TRUE               ~ 1L            
           ),
           .names = "{.col}_bin"
    ),

    curation_none = dplyr::case_when(
      is.na(curation_7)      ~ NA_integer_,
      curation_7 == 90       ~ 1L,    
      curation_7 == 0        ~ 0L,
      TRUE                   ~ NA_integer_
    )
  )

# --- Indices ---------------------------------------------------------------

data_idx <- data_rev %>%
  # Perceived usefulness
  add_index(idx_implicit,
            implicit_1, implicit_2_rev, implicit_3, implicit_4_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_explicit,
            explicit_1, explicit_2, explicit_3, explicit_4_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_incidentalness,
            incidentalness_1, incidentalness_2_rev, incidentalness_3, incidentalness_4, 
            incidentalness_5_rev,
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
            curation_1_bin, curation_2_bin, curation_3_bin, curation_4_bin,
            curation_5_bin, curation_6_bin,
            cast.numeric = TRUE) %>%
  # Information use (2-item indices)
  add_index(idx_info_undirected, undirected_news, undirected_life,           
            cast.numeric = TRUE) %>%
  add_index(idx_info_topic,      topic_interests, topic_hobbies,             
            cast.numeric = TRUE) %>%
  add_index(idx_info_problem,    problem_specific_need, problem_solving,           
            cast.numeric = TRUE) %>%
  add_index(idx_info_group,      group_close, group_extended,            
            cast.numeric = TRUE)

# --- Reliability (psych) ---------------------------------------------------

# ωh (drop-one only if ωh < .70) for perceptions and practices
pp_scales <- list(
  implicit       = c("implicit_1","implicit_2_rev","implicit_3","implicit_4_rev"),
  explicit       = c("explicit_1","explicit_2","explicit_3","explicit_4_rev"),
  incidentalness = c("incidentalness_1","incidentalness_2_rev","incidentalness_3",
                     "incidentalness_4","incidentalness_5_rev"),
  sociality      = c("sociality_1","sociality_2_rev","sociality_3","sociality_4",
                     "sociality_5_rev"),
  snacking       = c("snacking_1","snacking_2_rev","snacking_3","snacking_4_rev",
                     "snacking_5","snacking_6_rev","snacking_7_rev","snacking_8"),
  engagement     = c("engagement_1","engagement_2","engagement_3","engagement_4"),
  curation       = c("curation_1_bin","curation_2_bin","curation_3_bin",
                     "curation_4_bin","curation_5_bin", "curation_6_bin")
)

omega_results <- imap_dfr(pp_scales, ~ {
  res <- choose_items_by_omega(data_rev, .x, thr = 0.7)
  tibble(
    scale       = .y,
    omega_h     = res$omega_h,
    items_used  = paste(res$items_used, collapse = ", "),
    dropped     = ifelse(is.na(res$dropped), "-", res$dropped)
  )
})

omega_results
# Note: 'explicit' and 'curation' remain < .70 even after drop-one; others ≥ .70

# Spearman-Brown for information uses
info_scales <- list(
  info_undirected = c("undirected_news","undirected_life"),
  info_topic      = c("topic_interests","topic_hobbies"),
  info_problem    = c("problem_specific_need","problem_solving"),
  info_group      = c("group_close","group_extended")
)

sb_results <- purrr::imap_dfr(info_scales, ~ {
  sb <- spearman_brown_2items(data_rev[[.x[1]]], data_rev[[.x[2]]])
  tibble::tibble(
    scale          = .y,
    items          = paste(.x, collapse = ", "),
    spearman_brown = round(sb, 3),
    pass           = !is.na(sb) && sb >= 0.7
  )
})

sb_results

# --- Recode gender & education ---------------------------------------------

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

data_idx <- data_idx %>%
  mutate(
    gender_binary = recode_factor(gender,
                                  "weiblich"  = "female",
                                  "männlich" = "non-female"
    ),
    gender_binary = factor(gender_binary, levels = c("non-female","female"))
  )

# Save scored data for regression -------------------------------------------

saveRDS(data_idx, file = file.path("01_Data", "social_media_2025_scored.rds"))


# --- Sociodemographic composition and sample characteristics----------------

# Age
tidycomm::describe(data_idx, age_years)

# Gender
tidycomm::tab_frequencies(data_idx, gender_binary)

# Education
tidycomm::tab_frequencies(data_idx, education_cat)

tidycomm::tab_frequencies(data_idx, school_degree)

# Income
tidycomm::tab_frequencies(data_idx, household_income)

# Social media use: usage frequency per platform
tidycomm::tab_frequencies(data_idx, facebook_usage)

tidycomm::tab_frequencies(data_idx, instagram_usage)

tidycomm::tab_frequencies(data_idx, x_usage)

tidycomm::tab_frequencies(data_idx, tiktok_usage)

tidycomm::tab_frequencies(data_idx, youtube_usage)

# Social media use: questionnaire platform
tidycomm::tab_frequencies(data_idx, platform_helper)

# Social media use: platforms used
platform_vars <- c("facebook_usage","instagram_usage","x_usage","tiktok_usage",
                   "youtube_usage")

data_idx <- data_idx %>%
  mutate(across(all_of(platform_vars),
                ~ if_else(between(to_num(.), 1, 7), 1L, 0L, missing = 0L),
                .names = "{.col}_use"))

platforms_used <- data_idx %>%
  mutate(id = row_number()) %>%
  select(id, ends_with("_use")) %>%
  pivot_longer(-id, names_to = "platform", values_to = "use") %>%
  filter(use == 1L) %>%
  mutate(platform = sub("_usage_use$", "", platform)) %>%
  group_by(id) %>%
  summarise(
    n_platforms_used = n(),
    combo = paste(sort(platform), collapse = " + "),
    .groups = "drop"
  )

top_combos <- platforms_used %>%
  count(combo, sort = TRUE) %>%
  mutate(prop = round(n / sum(n), 3))

top_combos

# --- RQ1: Descriptive Analysis of Usefulness Perceptions--------------------

tidycomm::describe(data_idx, idx_implicit)

tidycomm::describe(data_idx, idx_explicit)

tidycomm::describe(data_idx, idx_incidentalness)

tidycomm::describe(data_idx, idx_sociality)

# Explorative Analysis: Differences between platforms + users

# --- RQ2: Descriptive Analysis of Practices --------------------------------

tidycomm::describe(data_idx, idx_engagement)

tidycomm::describe(data_idx, idx_curation)

# Explorative Analysis: Differences between platforms + users


tidycomm::describe(data_idx, idx_snacking)
