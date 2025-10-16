# ============================================================
# File:   03_Descriptive_Analysis.R
# Project: Social Media as an Information Source for Older Adults
# Author: Martin Fischer
# Date:   2025-10-02
# Purpose:
#   - Build indices; reliability checks; recode variables
#   - Descriptive analyses (sample characteristics, RQ1, RQ2)
# ============================================================

# --- Setup ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
rm(list = ls())
gc()

pacman::p_load(tidyverse, psych, tidycomm, dplyr)

source("02_Scripts/Helpers.R")  # to_fac, to_num, omega_h, 
                                # choose_items_by_omega, spearman_brown_2items

# --- Load clean data --------------------------------------------------------

clean_data <- readRDS("01_Data/social_media_2025_clean_renamed.rds")

# --- Reverse-code items ----------------------------------------------------
# Note: reverse_scale creates *_rev columns; here 1<->5/1<->8 mapping
# Important: recoded *not* the initial reversed items since 
# Infas coded "1" with "fully agree" and "5" with "don't agree at all"
# and "1" with "several times a day" and "8" with "never"

data_rev <- clean_data %>% reverse_scale(implicit_1,
                             implicit_3,
                             explicit_1,
                             explicit_2,
                             explicit_3,
                             incidentalness_1,
                             incidentalness_3,
                             incidentalness_4,
                             sociality_1,
                             sociality_3,
                             sociality_4,
                             snacking_1,
                             snacking_3,
                             snacking_5,
                             snacking_8,
                             undirected_news,
                             undirected_life,
                             problem_specific_need,
                             problem_solving,
                             topic_interests,
                             topic_hobbies,
                             group_close,
                             group_extended,
                             ### select lower and upper bound for recode
                             lower_end = 5,
                             upper_end = 1)

data_rev <- data_rev %>% reverse_scale(engagement_1,
                                       engagement_2,
                                       engagement_3,
                                       engagement_4,
                                       facebook_usage,
                                       instagram_usage,
                                       tiktok_usage,
                                       x_usage,
                                       youtube_usage,
                                       lower_end = 8,
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
            implicit_1_rev, 
            implicit_2, 
            implicit_3_rev,
            implicit_4,
            cast.numeric = TRUE) %>%
  add_index(idx_explicit,
            explicit_1_rev, 
            explicit_2_rev, 
            explicit_3_rev, 
            explicit_4,
            cast.numeric = TRUE) %>%
  add_index(idx_incidentalness,
            incidentalness_1_rev, 
            incidentalness_2,
            incidentalness_3_rev, 
            incidentalness_4_rev,
            incidentalness_5,
            cast.numeric = TRUE) %>%
  add_index(idx_sociality,
            sociality_1_rev, 
            sociality_2, 
            sociality_3_rev, 
            sociality_4_rev, 
            sociality_5, 
            cast.numeric = TRUE) %>%
  # Practices
  add_index(idx_snacking, 
            snacking_1_rev, 
            snacking_2, 
            snacking_3_rev, 
            snacking_4,
            snacking_5_rev, 
            snacking_6, 
            snacking_7,
            snacking_8_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_engagement, 
            engagement_1_rev, 
            engagement_2_rev, 
            engagement_3_rev,
            engagement_4_rev,
            cast.numeric = TRUE) %>%
  add_index(idx_curation, 
            curation_1_bin, 
            curation_2_bin, 
            curation_3_bin,
            curation_4_bin,
            curation_5_bin, 
            curation_6_bin,
            cast.numeric = TRUE,
            type = "sum") %>%
  # Information use (2-item indices)
  add_index(idx_info_undirected, 
            undirected_news_rev,
            undirected_life_rev,           
            cast.numeric = TRUE) %>%
  add_index(idx_info_topic,     
            topic_interests_rev,
            topic_hobbies_rev,             
            cast.numeric = TRUE) %>%
  add_index(idx_info_problem,    
            problem_specific_need_rev, 
            problem_solving_rev,           
            cast.numeric = TRUE) %>%
  add_index(idx_info_group,      
            group_close_rev, 
            group_extended_rev,            
            cast.numeric = TRUE)

# --- Reliability -----------------------------------------------------------

# ωh (drop-one only if ωh < .70) for perceptions and practices
pp_scales <- list(
  implicit       = c("implicit_1_rev",
                     "implicit_2",
                     "implicit_3_rev",
                     "implicit_4"),
  explicit       = c("explicit_1_rev",
                     "explicit_2_rev",
                     "explicit_3_rev",
                     "explicit_4"),
  incidentalness = c("incidentalness_1_rev",
                     "incidentalness_2",
                     "incidentalness_3_rev",
                     "incidentalness_4_rev",
                     "incidentalness_5"),
  sociality      = c("sociality_1_rev",
                     "sociality_2",
                     "sociality_3_rev",
                     "sociality_4_rev",
                     "sociality_5"),
  snacking       = c("snacking_1_rev",
                     "snacking_2",
                     "snacking_3_rev",
                     "snacking_4",
                     "snacking_5_rev",
                     "snacking_6",
                     "snacking_7",
                     "snacking_8_rev"),
  engagement     = c("engagement_1_rev",
                     "engagement_2_rev",
                     "engagement_3_rev",
                     "engagement_4_rev"),
  curation       = c("curation_1_bin",
                     "curation_2_bin",
                     "curation_3_bin",
                     "curation_4_bin",
                     "curation_5_bin", 
                     "curation_6_bin")
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
  info_undirected = c("undirected_news_rev","undirected_life_rev"),
  info_topic      = c("topic_interests_rev","topic_hobbies_rev"),
  info_problem    = c("problem_specific_need_rev","problem_solving_rev"),
  info_group      = c("group_close_rev","group_extended_rev")
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

# Age (incl. young-olds vs. old-olds)
tidycomm::describe(data_idx, age_years)

data_idx <- data_idx %>%
  mutate(
    age_binary = case_when(
      between(age_years, 60, 69) ~ "young-old",
      between(age_years, 70, 90) ~ "old-old"),
    age_binary = factor(age_binary, levels = c("young-old","old-old"))
  )

tidycomm::tab_frequencies(data_idx, age_binary)

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
# Binary, never (0) vs. at least less than once a month (1)

platform_vars <- c("facebook_usage_rev",
                   "instagram_usage_rev",
                   "x_usage_rev",
                   "tiktok_usage_rev",
                   "youtube_usage_rev")

data_idx <- data_idx %>%
  mutate(across(all_of(platform_vars),
                ~ if_else(between(to_num(.), 2, 8), 1L, 0L, missing = 0L),
                .names = "{.col}_overall_use"))

platform_use_vars  <- paste0(platform_vars, "_overall_use")

platforms_used <- data_idx %>%
  mutate(id = row_number()) %>%
  select(id, all_of(platform_use_vars)) %>%
  pivot_longer(-id, names_to = "platform", values_to = "use") %>%
  filter(use == 1L) %>%
  mutate(platform = sub("_usage_rev_overall_use$", "", platform)) %>%
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

# Social media use: sociodemographic differences between platforms ----------

anova_age <-data_idx %>% 
  unianova(platform_helper, age_years, descriptives = TRUE, post_hoc = TRUE)

View(anova_age)

data_idx %>% 
  crosstab(platform_helper, age_binary, chi_square = TRUE)

data_idx %>% 
  crosstab(platform_helper, gender_binary, chi_square = TRUE)

data_idx %>% 
  crosstab(education_cat, platform_helper, chi_square = TRUE)

# --- RQ1: Descriptive Analysis of Usefulness Perceptions--------------------

tidycomm::describe(data_idx, idx_implicit)

tidycomm::describe(data_idx, idx_explicit)

tidycomm::describe(data_idx, idx_incidentalness)

tidycomm::describe(data_idx, idx_sociality)

# --- RQ2: Descriptive Analysis of Practices --------------------------------

# Engagement: Mean index & frequencies of individual engagement practices
tidycomm::describe(data_idx, idx_engagement)

engagement_vars <- c("engagement_1_rev","engagement_2_rev",
                     "engagement_3_rev","engagement_4_rev")

data_idx <- data_idx %>%
  mutate(across(
    all_of(engagement_vars),
    ~ case_when(
      between(as.integer(.), 5, 8) ~ "often",
      between(as.integer(.), 2, 4) ~ "rarely",
      as.integer(.) == 1           ~ "never",
      TRUE                         ~ NA_character_
    ),
    .names = "{.col}_overall_use"
  ))

engagement_overall_vars <- c("engagement_1_rev_overall_use",
                             "engagement_2_rev_overall_use",
                             "engagement_3_rev_overall_use",
                             "engagement_4_rev_overall_use")

engagement_frequencies <- data_idx %>%
  select(all_of(engagement_overall_vars)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "value") %>%
  filter(!is.na(value)) %>%                 
  count(item, value, name = "n") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = value,
              values_from = c(n, percent),
              names_prefix = "val_")

engagement_frequencies

# Curation: Mean index & frequencies of individual curation practices
tidycomm::describe(data_idx, idx_curation)

curation_vars <- paste0("curation_", 1:6, "_bin")

curation_frequencies <- data_idx %>%
  select(all_of(curation_vars)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "value") %>%
  filter(!is.na(value)) %>%                 
  count(item, value, name = "n") %>%
  group_by(item) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = value,
              values_from = c(n, percent),
              names_prefix = "val_")

curation_frequencies

# Snacking: Mean index
tidycomm::describe(data_idx, idx_snacking)