# ============================================================
# File:   04a_Regression_RQ3.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Regression analyses of social media survey dataset
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

# -------------------------
# Create Directories -------
# -------------------------
dir.create("04_Figures", showWarnings = FALSE)
dir.create("04_Figures/Individual_Scatter_Plots", showWarnings = FALSE)
dir.create("04_Figures/Facet_Grids", showWarnings = FALSE)
dir.create("04_Figures/RQ3", showWarnings = FALSE)
dir.create("03_Output", showWarnings = FALSE)
dir.create("03_Output/RQ3", showWarnings = FALSE)

# -------------------------
# Define Indices ----------
# -------------------------
predictor_indices <- c(
  "idx_implicit", "idx_explicit", "idx_sociality", "idx_incidentalness",
  "idx_snacking", "idx_engagement", "idx_curation"
)

info_indices <- c(
  "idx_info_undirected", "idx_info_topic", "idx_info_problem", "idx_info_group"
)

info_labels <- c(
  "Undirected Information Use",
  "Topic-related Information Use",
  "Problem-related Information Use",
  "Group-related Information Use"
)


# -------------------------
# 1. Scatterplots & Facet Grids
# -------------------------
for(pred in predictor_indices){
  # Individual Scatterplots
  for(dv in info_indices){
    p <- ggplot(df, aes_string(x = pred, y = dv)) +
      geom_jitter(width = 0.1, height = 0.1, alpha = 0.6) +
      geom_smooth(method = "loess", se = TRUE, color = "blue") +
      labs(title = paste("Scatterplot:", gsub("_", " ", pred), "vs", gsub("_", " ", dv)),
           x = gsub("_", " ", pred), y = gsub("_", " ", dv)) +
      theme_minimal()
    
    ggsave(filename = paste0("04_Figures/Individual_Scatter_Plots/", pred, "_vs_", dv, ".png"),
           plot = p, width = 14, height = 10, dpi = 300)
  }
  
  # Facet Grids
  df_long <- df %>%
    pivot_longer(cols = all_of(info_indices), names_to = "DV", values_to = "value")
  
  p <- ggplot(df_long, aes_string(x = pred, y = "value")) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    facet_wrap(~DV, scales = "free_y") +
    labs(title = paste("Facet-Grid:", gsub("_", " ", pred), "vs Information Use Indices"),
         x = gsub("_", " ", pred), y = "Information Use") +
    theme_minimal()
  
  ggsave(filename = paste0("04_Figures/Facet_Grids/", pred, "_facetgrid_info.png"),
         plot = p, width = 20, height = 12, dpi = 300)
}

# -------------------------
# 2. Correlation Plots
# -------------------------
# Substantial predictors (for regression)
df_corr <- df %>%
  select(age_years, gender_binary, education_cat, platform_helper,
         idx_implicit, idx_explicit, idx_sociality, idx_incidentalness) %>%
  mutate(across(where(is.factor), as.numeric))

custom_labels <- c(
  "Age", "Gender (female)", "Education", "Platform",
  "Implicit Personalization", "Explicit Personalization",
  "Sociality", "Incidentalness"
)

cplot <- ggcorrplot(
  cor(df_corr, use = "pairwise.complete.obs"),
  method = "square",
  ggtheme = theme_minimal(base_family = "Times New Roman"),
  title = "Correlation Among Model Predictors",
  type = "full",
  lab = TRUE
) +
  scale_x_discrete(labels = custom_labels) +
  scale_y_discrete(labels = custom_labels)

ggsave(filename = "04_Figures/RQ3/corrplot_model_predictors.png",
       plot = cplot, width = 2000, height = 2000, units = "px")

# -------------------------
# 3. Regression Models (RQ3)
# -------------------------
model_list <- list()

for(dv in info_indices){
  # Block 0
  m0 <- lm(as.formula(paste(dv, "~ age_years + gender_binary + education_cat + platform_helper + platform_usage")), data = df)
  # Block 1
  m1 <- update(m0, . ~ . + idx_implicit + idx_explicit + idx_sociality + idx_incidentalness)
  
  model_list[[paste0(dv, "_0")]] <- m0
  model_list[[paste0(dv, "_1")]] <- m1
}

# Function: standardize models
get_std_models <- function(models) lapply(models, lm.beta)

# -------------------------
# 4. Markdown output per DV
# -------------------------
for(dv in info_indices){
  models_to_report <- list(model_list[[paste0(dv, "_0")]], model_list[[paste0(dv, "_1")]])
  models_std <- get_std_models(models_to_report)
  
  md_file <- paste0("03_Output/RQ3/Models_", dv, ".md")
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

# -------------------------
# 5. HTML combined tables
# -------------------------
html_file <- "03_Output/RQ3/All_Info_Models.html"
cat('<html>
<head>
<title>Regression Results</title>
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

for(i in seq_along(info_indices)){
  dv <- info_indices[i]
  dv_label <- info_labels[i]
  models_std <- get_std_models(list(model_list[[paste0(dv, "_0")]], model_list[[paste0(dv, "_1")]]))
  
  cat(paste0('<div class="table-box"><h2>', dv_label, '</h2>\n'), file = html_file, append = TRUE)
  
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

# -------------------------
# 6. Robust SE for Block 1
# -------------------------
robust_beta_list <- list()

for(dv in info_indices){
  model <- model_list[[paste0(dv, "_1")]]  # Original lm
  # Robuste SE
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  
  # Standardisierte Beta
  std_beta <- lm.beta(model)$standardized.coefficients
  
  coef_df <- tibble(
    DV = dv,
    Term = names(std_beta),
    StdBeta = round(std_beta, 3),
    Robust_SE = round(robust_se, 3),
    t_value = round(std_beta / robust_se, 3),
    p_value = round(2 * pt(-abs(std_beta / robust_se), df = model$df.residual), 3)
  )
  
  # In Liste speichern
  robust_beta_list[[dv]] <- coef_df
}

# Optional: alle Tabellen zusammenführen
robust_beta_all <- bind_rows(robust_beta_list)

# Oder in Markdown speichern
robust_beta_all %>% 
  knitr::kable(format = "markdown") %>% 
  cat(file = "03_Output/RQ3/Robust_StdBetas_Block1.md", sep = "\n")


# -------------------------
# 7. Check model requirements und speichern
# -------------------------

# Liste für alle Modelle
requirements_list <- list()

for(dv in info_indices){
  models <- list(model_list[[paste0(dv, "_0")]], model_list[[paste0(dv, "_1")]])
  for(i in seq_along(models)){
    res <- check_requirements_md(models[[i]])
    res <- res %>% mutate(DV = dv, Block = paste0("Block ", i-1))
    requirements_list[[paste0(dv, "_", i-1)]] <- res
  }
}

# Zusammenführen
requirements_all <- bind_rows(requirements_list)

# In Markdown speichern
requirements_all %>%
  select(DV, Block, Shapiro_Wilk_p, Shapiro_Wilk_Result,
         Max_VIF, VIF_Result, Breusch_Pagan_p, BP_Result, Model_Call) %>%
  knitr::kable(format = "markdown") %>%
  cat(file = "03_Output/RQ3/Model_Requirements.md", sep = "\n")


# -------------------------
# End of RQ3
# -------------------------
