# ============================================================
# File:   04a_Regression_RQ4.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-10-01
# Purpose:
#   - Hierarchical regression analyses for RQ4
#   - All outputs use standardized Beta coefficients
# ============================================================

# -------------------------
# Packages ----------------
# -------------------------
rm(list = ls(all = TRUE))
if(!require(pacman)) install.packages("pacman")
pacman::p_unload("all")

pacman::p_load(
  tidyverse, report, texreg, dplyr, ggcorrplot, viridis, extrafont, 
  lm.beta, car, ggplot2, ggfortify, lmtest, GGally, ggeffects,
  knitr, kableExtra, sandwich, htmltools
)

# Load helper functions (to_fac, to_num)
source("02_Scripts/Helpers.R")  # Load functions


# -------------------------
# Fonts -------------------
# -------------------------
font_import(paths = "C:/WINDOWS/Fonts", pattern = "times.ttf", prompt = FALSE)
loadfonts(device = "win")

# -------------------------
# Load Data ----------------
# -------------------------
df <- readRDS("01_Data/social_media_2025_scored.rds")

# -------------------------
# Create Directories -------
# -------------------------
dir.create("04_Figures/RQ4", showWarnings = FALSE)
dir.create("03_Output/RQ4", showWarnings = FALSE)

# -------------------------
# Define DVs and Labels ----
# -------------------------
rq4_dvs <- c(
  "Navigation Practices" = "idx_snacking",
  "Engagement Practices" = "idx_engagement",
  "Curation Practices" = "idx_curation"
)

# -------------------------
# Create platform-specific usage variable
# -------------------------
df <- df %>%
  mutate(platform_usage = case_when(
    platform_helper == 1 ~ facebook_usage,   # Facebook
    platform_helper == 2 ~ instagram_usage,  # Instagram
    platform_helper == 3 ~ x_usage,          # X (ehemals Twitter)
    platform_helper == 4 ~ tiktok_usage,     # TikTok
    platform_helper == 5 ~ youtube_usage,    # YouTube
    TRUE ~ NA_real_                           # sonst NA
  ))
df <- df %>%
  mutate(platform_helper = factor(platform_helper,
                                  levels = 1:5,
                                  labels = c("Facebook", "Instagram", "X", "TikTok", "YouTube")))


# -------------------------
# 1. Regression Models RQ4
# -------------------------
model_list <- list()

for(dv_name in names(rq4_dvs)){
  dv <- rq4_dvs[dv_name]
  
  # Block 1: Demographics + Platform + Usage
  f1 <- as.formula(paste(dv, "~ age_years + gender_binary + education_cat + platform_helper + platform_usage"))
  m1 <- lm(f1, data = df)
  
  # Block 2: Add perceived usefulness
  f2 <- update(f1, . ~ . + idx_implicit + idx_explicit + idx_sociality + idx_incidentalness)
  m2 <- lm(f2, data = df)
  
  # Block 3: Add information use frequencies
  f3 <- update(f2, . ~ . + idx_info_undirected + idx_info_topic + idx_info_group + idx_info_problem)
  m3 <- lm(f3, data = df)
  
  # Store models
  model_list[[paste0(dv, "_1")]] <- m1
  model_list[[paste0(dv, "_2")]] <- m2
  model_list[[paste0(dv, "_3")]] <- m3
}

# Function: standardize models
get_std_models <- function(models) lapply(models, lm.beta)

# -------------------------
# 2. Markdown output per DV
# -------------------------
for(dv_name in names(rq4_dvs)){
  dv <- rq4_dvs[dv_name]
  models_to_report <- list(model_list[[paste0(dv, "_1")]],
                           model_list[[paste0(dv, "_2")]],
                           model_list[[paste0(dv, "_3")]])
  models_std <- get_std_models(models_to_report)
  
  md_file <- paste0("03_Output/RQ4/Models_", dv, ".md")
  cat("---\n",
      "title: 'Regression Results for ", dv, "'\n",
      "output: html_document\n",
      "---\n\n", file = md_file)
  
  # Adjusted R²
  r2_table <- tibble(
    Block = c("Block 1", "Block 2", "Block 3"),
    Adj_R2 = sapply(models_std, function(m) round(summary(m)$adj.r.squared, 3))
  )
  cat("## Adjusted R²\n\n", file = md_file, append = TRUE)
  r2_table %>% kable(format = "markdown") %>% cat(file = md_file, append = TRUE, sep = "\n")
  
  # Model coefficients
  cat("\n\n## Model Coefficients (Standardized Betas)\n\n", file = md_file, append = TRUE)
  texreg_txt <- capture.output(texreg::screenreg(models_std, single.row = TRUE, digits = 3))
  cat(texreg_txt, sep = "\n", file = md_file, append = TRUE)
  
  # Detailed coefficients table
  coef_list <- lapply(seq_along(models_std), function(i){
    s <- summary(models_std[[i]])
    tibble(
      Block = paste0("Block ", i),
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

# -------------------------
# 3. HTML combined tables
# -------------------------
html_file <- "03_Output/RQ4/All_RQ4_Models.html"
cat('<html>
<head>
<title>Regression Results RQ4</title>
<style>
.container { display: flex; flex-wrap: wrap; justify-content: space-around; }
.table-box { flex: 0 0 45%; margin-bottom: 20px; }
h2 { text-align: center; }
</style>
</head>
<body>
<div class="container">
', file = html_file)

for(dv_name in names(rq4_dvs)){
  dv <- rq4_dvs[dv_name]
  models_std <- get_std_models(list(
    model_list[[paste0(dv, "_1")]],
    model_list[[paste0(dv, "_2")]],
    model_list[[paste0(dv, "_3")]]
  ))
  
  cat(paste0('<div class="table-box"><h2>', dv_name, '</h2>\n'), file = html_file, append = TRUE)
  
  tmp <- tempfile(fileext = ".html")
  texreg::htmlreg(models_std, single.row = TRUE, digits = 2,
                  custom.header = setNames(list(1:3), dv_name),
                  custom.model.names = c("Block 1", "Block 2", "Block 3"), file = tmp)
  
  table_html <- readLines(tmp)
  table_html <- table_html[grep("<table", table_html):grep("</table>", table_html)]
  cat(paste0(table_html, collapse = "\n"), "\n", file = html_file, append = TRUE)
  unlink(tmp)
  
  cat('</div>\n', file = html_file, append = TRUE)
}

cat('</div>\n</body>\n</html>', file = html_file, append = TRUE)

# -------------------------
# 4. Robust SE for Block 3 (Standardized Betas)
# -------------------------
robust_beta_list <- list()

for(dv_name in names(rq4_dvs)){
  dv <- rq4_dvs[dv_name]
  model <- model_list[[paste0(dv, "_3")]]  # Block 3 model
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  std_beta <- lm.beta(model)$standardized.coefficients
  
  coef_df <- tibble(
    DV = dv,
    Term = names(std_beta),
    StdBeta = round(std_beta, 3),
    Robust_SE = round(robust_se, 3),
    t_value = round(std_beta / robust_se, 3),
    p_value = round(2 * pt(-abs(std_beta / robust_se), df = model$df.residual), 3)
  )
  
  robust_beta_list[[dv]] <- coef_df
}

robust_beta_all <- bind_rows(robust_beta_list)

robust_beta_all %>% 
  knitr::kable(format = "markdown") %>% 
  cat(file = "03_Output/RQ4/Robust_StdBetas_Block3.md", sep = "\n")

# -------------------------
# 5. Check model requirements und speichern
# -------------------------

# Liste für alle Modelle
requirements_list <- list()

for(dv_name in names(rq4_dvs)){
  dv <- rq4_dvs[dv_name]
  models <- list(
    model_list[[paste0(dv, "_1")]],
    model_list[[paste0(dv, "_2")]],
    model_list[[paste0(dv, "_3")]]
  )
  
  for(i in seq_along(models)){
    res <- check_requirements_md(models[[i]])
    res <- res %>% mutate(DV = dv, Block = paste0("Block ", i))
    requirements_list[[paste0(dv, "_", i)]] <- res
  }
}

# Zusammenführen
requirements_all <- bind_rows(requirements_list)

# Markdown speichern
requirements_all %>%
  select(DV, Block, Shapiro_Wilk_p, Shapiro_Wilk_Result,
         Max_VIF, VIF_Result, Breusch_Pagan_p, BP_Result, Model_Call) %>%
  knitr::kable(format = "markdown") %>%
  cat(file = "03_Output/RQ4/Model_Requirements.md", sep = "\n")

# -------------------------
# End of RQ4
# -------------------------
