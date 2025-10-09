# ============================================================
# File:   05c_Explorative_Analysis_RQ1.R
# Project: Social Media as an Information Source for Older Adults
# Author: Martin Fischer
# Date:   2025-10-09
# Purpose:
#   - Explorative Analysis RQ1 (user- and platform-differences)
# ============================================================

# --- Setup -----------------------------------------------------------------

rm(list = ls(all = TRUE))
if(!require(pacman)) install.packages("pacman")
pacman::p_unload("all")

pacman::p_load(
  tidyverse, texreg, lm.beta, sandwich, lmtest, kableExtra,
  ggcorrplot, car, knitr, extrafont, tidycomm, emmeans)

source("02_Scripts/Helpers.R")  # to_fac, to_num

# --- Fonts (only for Windows) ----------------------------------------------

# font_import(paths = "C:/WINDOWS/Fonts", pattern = "times.ttf", prompt = FALSE)
# loadfonts(device = "win")

# --- Data ------------------------------------------------------------------

df <- readRDS("01_Data/social_media_2025_scored.rds")

# --- Platform controls -----------------------------------------------------
df <- df %>%
  mutate(platform_usage = case_when(
    platform_helper == 1 ~ facebook_usage_rev,   # Facebook
    platform_helper == 2 ~ instagram_usage_rev,  # Instagram
    platform_helper == 5 ~ youtube_usage_rev,    # YouTube
    TRUE ~ NA_real_                              # X/TikTok --> NA
  )) %>% 
  mutate(platform_helper = factor(platform_helper,
                                  levels = c(2, 1, 5),
                                  labels = c("Instagram", "Facebook", "YouTube")))

# --- Directory -----------------------------------------------------------

dir.create("03_Output/Explorative_Analyses/RQ1", showWarnings = FALSE)

# --- Indices ---------------------------------------------------------------

dv_indices <- c(
  "idx_implicit", "idx_explicit", "idx_sociality", "idx_incidentalness"
)

dv_labels <- c(
  "Implicit Personalization",
  "Explicit Personalization",
  "Sociality",
  "Incidentality"
)

# --- 1. Regression Models --------------------------------------------------

model_list <- list()

for(dv in dv_indices){
  # Block 0: SD
  m0 <- lm(as.formula(paste(dv, "~ age_years + gender_binary + education_cat")), 
           data = df)
  # Block 1: + platform
  m1 <- update(m0, . ~ . + platform_helper + platform_usage)
  
  model_list[[paste0(dv, "_0")]] <- m0
  model_list[[paste0(dv, "_1")]] <- m1
}

# Standardize models
get_std_models <- function(models) lapply(models, lm.beta)

# --- 4. Markdown output per DV ---------------------------------------------

for(dv in dv_indices){
  models_to_report <- list(model_list[[paste0(dv, "_0")]], model_list[[paste0(dv, "_1")]])
  models_std <- get_std_models(models_to_report)
  
  md_file <- paste0("03_Output/Explorative_Analyses/RQ1/Models_", dv, ".md")
  cat("---\n",
      "title: 'Regression Results for ", dv, "'\n",
      "output: html_document\n",
      "---\n\n", file = md_file)
  
  # Adjusted R²
  r2_table <- tibble(
    Block = c("Block 0", "Block 1"),
    Adj_R2 = sapply(models_std, function(m) round(summary(m)$adj.r.squared, 3))
  )
  cat("## Adjusted R²\n\n", file = md_file, append = TRUE)
  r2_table %>% kable(format = "markdown") %>% cat(file = md_file, append = TRUE, sep = "\n")
  
  # Model coefficients
  cat("\n\n## Model Coefficients (Standardized Betas)\n\n", file = md_file, append = TRUE)
  texreg_txt <- capture.output(texreg::screenreg(models_std, single.row = TRUE, digits = 3))
  cat(texreg_txt, sep = "\n", file = md_file, append = TRUE)
  
  #Posthoc pairwise platform comparisons
  m_full <- model_list[[paste0(dv, "_1")]]
  emm <- emmeans(m_full, ~ platform_helper,
                 vcov. = sandwich::vcovHC(m_full, type = "HC3"))
  ph <- pairs(emm, adjust = "tukey") |> as.data.frame()
  cat("\n\n## Post-hoc comparisons (adjusted)\n\n", file = md_file, append = TRUE)
  knitr::kable(ph, format = "markdown", digits = 3) |>
    paste(collapse = "\n") |>
    cat(file = md_file, append = TRUE, sep = "\n")
  es <- eff_size(emm, method = "pairwise",
                 sigma = sigma(m_full), edf = df.residual(m_full)) |>
    as.data.frame()
  cat("\n\n## Effect sizes (Cohen's d)\n\n", file = md_file, append = TRUE)
  knitr::kable(es, format = "markdown", digits = 3) |>
    paste(collapse = "\n") |>
    cat(file = md_file, append = TRUE, sep = "\n")
  
  # Detailed coefficients table
  coef_list <- lapply(seq_along(models_std), function(i){
    s <- summary(models_std[[i]])
    tibble(
      Block = paste0("Block ", i-1),
      Term = rownames(s$coefficients),
      StdBeta = round(s$coefficients[, "Standardized"], 3),
      StdError = round(s$coefficients[, "Std. Error"], 3),
      t_value = round(s$coefficients[, "t value"], 3),
      p_value = round(s$coefficients[, "Pr(>|t|)"], 3)
    )
  })
  coef_df <- bind_rows(coef_list)
  
  cat("\n\n## Detailed Coefficients (Standardized Betas)\n\n", file = md_file, append = TRUE)
  coef_df %>% kable(format = "markdown") %>% cat(file = md_file, append = TRUE, sep = "\n")
}

# --- 5. HTML combined tables -----------------------------------------------

html_file <- "03_Output/Explorative_Analyses/RQ1/All_Perception_Models.html"
cat('<html>
<head>
<title>Regression Results RQ1 - Explorative Analysis</title>
<style>
.container {
  display: flex;
  flex-wrap: wrap;
  justify-content: space-around;
}
.table-box {
  flex: 0 0 45%;
  margin-bottom: 20px;
}
h2 { text-align: center; }
</style>
</head>
<body>
<div class="container">
', file = html_file)

for(i in seq_along(dv_indices)){
  dv <- dv_indices[i]
  dv_label <- dv_labels[i]
  models_std <- get_std_models(list(model_list[[paste0(dv, "_0")]], 
                                    model_list[[paste0(dv, "_1")]]))
  
  cat(paste0('<div class="table-box"><h2>', dv_label, '</h2>\n'), 
      file = html_file, append = TRUE)
  
  tmp <- tempfile(fileext = ".html")
  texreg::htmlreg(models_std, single.row = TRUE, digits = 2,
                  custom.header = setNames(list(1:2), dv_label),
                  custom.model.names = c("Block 0", "Block 1"), file = tmp)
  
  table_html <- readLines(tmp)
  table_html <- table_html[grep("<table", table_html):grep("</table>", table_html)]
  cat(paste0(table_html, collapse = "\n"), "\n", file = html_file, append = TRUE)
  unlink(tmp)
  
  cat('</div>\n', file = html_file, append = TRUE)
}

cat('</div>\n</body>\n</html>', file = html_file, append = TRUE)

# --- 6. Robust SE for Block 1 ----------------------------------------------

robust_beta_list <- list()

for(dv in dv_indices){
  model <- model_list[[paste0(dv, "_1")]]  # Original lm
  # Robust SE
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  
  # Standardized Beta
  std_beta <- lm.beta(model)$standardized.coefficients
  
  coef_df <- tibble(
    DV = dv,
    Term = names(std_beta),
    StdBeta = round(std_beta, 3),
    Robust_SE = round(robust_se, 3),
    t_value = round(std_beta / robust_se, 3),
    p_value = round(2 * pt(-abs(std_beta / robust_se), df = model$df.residual), 3)
  )
  
  # Save in list
  robust_beta_list[[dv]] <- coef_df
}

# Optional: bind all tables
robust_beta_all <- bind_rows(robust_beta_list)

# Or save in Markdown
robust_beta_all %>% 
  knitr::kable(format = "markdown") %>% 
  cat(file = "03_Output/Explorative_Analyses/RQ1/Robust_StdBetas_Block1.md", sep = "\n")

# --- 7. Check model requirements ----------------------------------

# List for all models
requirements_list <- list()

for(dv in dv_indices){
  models <- list(model_list[[paste0(dv, "_0")]], model_list[[paste0(dv, "_1")]])
  for(i in seq_along(models)){
    res <- check_requirements_md(models[[i]])
    res <- res %>% mutate(DV = dv, Block = paste0("Block ", i-1))
    requirements_list[[paste0(dv, "_", i-1)]] <- res
  }
}

#Bind and save overview
requirements_all <- bind_rows(requirements_list)

requirements_all %>%
  select(DV, Block, Shapiro_Wilk_p, Shapiro_Wilk_Result,
         Max_VIF, VIF_Result, Breusch_Pagan_p, BP_Result, Model_Call) %>%
  knitr::kable(format = "markdown") %>%
  cat(file = "03_Output/Explorative_Analyses/RQ1/Model_Requirements.md", sep = "\n")

# --- Additional Check: Usage intensity differences between platforms -------

df %>% 
  filter(platform_helper %in% c("Facebook", "Instagram", "YouTube")) %>% 
  dplyr::group_by(platform_helper) %>% 
  describe(platform_usage)
  
usage_anova <-  df %>% 
  filter(platform_helper %in% c("Facebook", "Instagram", "YouTube")) %>% 
  unianova(platform_helper, platform_usage, descriptives = TRUE, post_hoc = TRUE)

View(usage_anova)

# --- End of Script ------------------------------------------------------------
