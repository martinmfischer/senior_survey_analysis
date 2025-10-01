# ============================================================
# File:   04_Regression.R
# Project: LMU Social Media 2025
# Author: Martin Fischer
# Date:   2025-09-12
# Purpose:
#   - Regression analyses of social media survey dataset
#   - Example: linear model predicting a social media attitude variable
# ============================================================
# Loading Packages and Data --------------------------
# Load packages

rm(list = ls(all = TRUE))
if(!require(pacman)) install.packages("pacman")
pacman::p_unload("all")

pacman::p_load(
  "tidyverse",
  "tidycomm",
  "report",
  "texreg",
  "dplyr",
  "ggcorrplot",
  "viridis",
  "extrafont",
  "lm.beta",
  "car",
  "formula.tools",
  "ggplot2",
  "ggfortify",
  "lmtest",
  "GGally",
  "ggeffects",
  "knitr",
  "kableExtra",
  "sandwich",
  "lmtest",
  "htmltools"
)



#### create function to check data for requirements ####
check_requirements <- function(lm_model) {
  
  #lm_model <- m1.0
  data <- lm_model$model
  
  data <- data %>% mutate(across(where(is.factor), ~ as.numeric(.)))
  
  # 1. Shapiro-Wilk-Test auf Normalverteilung der Residuen
  shapiro_test <- shapiro.test(residuals(lm_model))
  shapiro_result <- ifelse(shapiro_test$p.value > 0.05, 
                           "Normalverteilung nicht verletzt (gut)", 
                           "Normalverteilung verletzt (SCHLECHT)")
  #cat("Shapiro-Wilk Test auf Normalverteilung der Residuen:\n")
  #cat("\nInterpretation: ", shapiro_result, "\n\n")
  
  # 2. VIF (Multikollinearität) - Variance Inflation Factor
  vif_values <- vif(lm_model)
  vif_result <- ifelse(any(vif_values > 10), 
                       "Multikollinearität vorhanden (SCHLECHT)", 
                       "Keine Multikollinearität (gut)")
  #cat("VIF (Variance Inflation Factor) Test:\n")
  #cat("\nInterpretation: ", vif_result, "\n\n")
  
  # 3. Breusch-Pagan-Test auf Heteroskedastizität
  bp_test <- bptest(lm_model)
  bp_result <- ifelse(bp_test$p.value > 0.05, 
                      "Keine Heteroskedastizität (gut)", 
                      "Heteroskedastizität vorhanden (SCHLECHT)")
  #cat("Breusch-Pagan Test auf Heteroskedastizität:\n")
  #cat("\nInterpretation: ", bp_result, "\n\n")
  print("-----------------------------------------------------")
  print(lm_model$call)
  print("::::")
  print(shapiro_result)
  print(vif_result)
  print(paste0("", bp_result, ""))
  print("-----------------------------------------------------")
}




# Load data


df <- readRDS("01_Data/social_media_2025_scored.rds")

# 
# 
# df_standardized <- df %>%
#   mutate(across(where(is.numeric), scale))
# df <- df_standardized


# Analysis of Data --------------------------------------------------------

if(!dir.exists("04_Figures")) dir.create("04_Figures")
if(!dir.exists("04_Figures/Individual_Scatter_Plots")) dir.create("04_Figures/Individual_Scatter_Plots")
if(!dir.exists("04_Figures/Facet_Grids")) dir.create("04_Figures/Facet_Grids")



# Listen der Indizes
predictor_indices <- c("idx_implicit", "idx_explicit", "idx_incidentalness", "idx_sociality",
                       "idx_snacking", "idx_engagement", "idx_curation")

info_indices <- c("idx_info_undirected", "idx_info_topic", "idx_info_problem", "idx_info_group")

# Schleife über alle Kombinationen - einzeln
for(pred in predictor_indices){
  for(dv in info_indices){
    # Plot erstellen
    p <- ggplot(df, aes_string(x = pred, y = dv)) +
      geom_jitter(width = 0.1, height = 0.1, alpha = 0.6) +
      geom_smooth(method = "loess", se = TRUE, color = "blue") +
      labs(
        title = paste("Scatterplot:", gsub("_", " ", pred), "vs", gsub("_", " ", dv)),
        x = gsub("_", " ", pred),
        y = gsub("_", " ", dv)
      ) +
      theme_minimal()
    
    # Dateiname erstellen
    file_name <- paste0("04_Figures/Individual_Scatter_Plots/", pred, "_vs_", dv, ".png")
    
    # Plot speichern
    ggsave(filename = file_name, plot = p, width = 14, height = 10, dpi = 300)
  }
}


# Schleife über Prädiktoren - facet grids
for(pred in predictor_indices){
  
  # Daten ins "long format" bringen für Facet
  df_long <- df %>%
    pivot_longer(
      cols = all_of(info_indices),
      names_to = "DV",
      values_to = "value"
    )
  
  # Plot erstellen
  p <- ggplot(df_long, aes_string(x = pred, y = "value")) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    facet_wrap(~DV, scales = "free_y") +
    labs(
      title = paste("Facet-Grid:", gsub("_", " ", pred), "vs Information Use Indices"),
      x = gsub("_", " ", pred),
      y = "Information Use"
    ) +
    theme_minimal()
  
  # Dateiname erstellen
  file_name <- paste0("04_Figures/Facet_Grids/", pred, "_facetgrid_info.png")
  
  # Plot speichern
  ggsave(filename = file_name, plot = p, width = 20, height = 12, dpi = 300)
}



corr_graph <- ggcorr(df[, c(predictor_indices, info_indices)], label = TRUE, label_round = 2)
ggsave(filename = "04_Figures/Corr_graph.png", plot = corr_graph, width = 7, height = 5, dpi = 300)

full_plot <- GGally::ggpairs(df[, c(predictor_indices[1:4], info_indices)])
ggsave(filename = "04_Figures/full_plot.png", plot = full_plot, width = 20, height = 20, dpi = 300)

##################################
# Start modeling, RQ3
##################################

if(!dir.exists("03_Output")) dir.create("03_Output")
if(!dir.exists("03_Output/RQ3")) dir.create("03_Output/RQ3")


# -------------------------
# Liste für alle Modelle speichern
# -------------------------
model_list <- list()

# -------------------------
# Schleife über alle DVs (Block 0 + Block 1)
# -------------------------
for(dv in info_indices){
  
  # Basismodell: Block 0 (Demografie + Plattform)
  base_formula <- as.formula(paste(dv, "~ age_years + gender_binary + education_cat + platform_helper"))
  m0 <- lm(base_formula, data = df)
  
  # Block 1: + Perceived Usefulness
  m1 <- update(m0, . ~ . + idx_implicit + idx_explicit + idx_sociality + idx_incidentalness)
  
  # Modelle in Liste speichern
  model_list[[paste0(dv, "_0")]] <- m0
  model_list[[paste0(dv, "_1")]] <- m1
  
  # -------------------------
  # Report Tabellen
  # -------------------------
  cat("\n--- REPORT:", dv, "Block 0 ---\n")
  print(report_table(m0))
  
  cat("\n--- REPORT:", dv, "Block 1 ---\n")
  print(report_table(m1))
  
  # -------------------------
  # Residualplots (optional, ggtitle für Übersicht)
  # -------------------------
  autoplot(m0) + ggtitle(paste("Residuals:", dv, "Block 0"))
  autoplot(m1) + ggtitle(paste("Residuals:", dv, "Block 1"))
}

# -------------------------
# Adj. R² für alle Modelle ausgeben
# -------------------------
for(name in names(model_list)){
  s <- summary(model_list[[name]])
  cat(name, "Adj. R²:", round(s$adj.r.squared, 3), "\n")
}

# -------------------------
# Texreg Tabellen pro DV
# -------------------------
for(dv in info_indices){
  models_to_report <- list(
    model_list[[paste0(dv, "_0")]],
    model_list[[paste0(dv, "_1")]]
  )
  
  cat("\n====================\n")
  cat("Texreg für:", dv, "\n")
  cat("====================\n\n")
  
  print(texreg::screenreg(models_to_report, single.row = TRUE, digits = 3))
}

# -------------------------
# Markdown-Ausgabe pro DV
# -------------------------
for(dv in info_indices){
  
  models_to_report <- list(
    model_list[[paste0(dv, "_0")]],
    model_list[[paste0(dv, "_1")]]
  )
  
  md_file <- paste0("03_Output/RQ3/Models_", dv, ".md")
  cat("---\n",
      "title: 'Regression Results for ", dv, "'\n",
      "output: html_document\n",
      "---\n\n", file = md_file)
  
  # Adjusted R²
  r2_table <- tibble(
    Block = c("Block 0", "Block 1"),
    Adj_R2 = sapply(models_to_report, function(m) round(summary(m)$adj.r.squared,3))
  )
  
  cat("## Adjusted R²\n\n", file = md_file, append = TRUE)
  r2_table %>% kable(format = "markdown") %>% 
    cat(file = md_file, append = TRUE, sep = "\n")
  
  # Texreg Tabelle
  cat("\n\n## Model Coefficients\n\n", file = md_file, append = TRUE)
  texreg_txt <- capture.output(texreg::screenreg(models_to_report, single.row = TRUE, digits = 3))
  cat(texreg_txt, sep = "\n", file = md_file, append = TRUE)
  
  # Detaillierte Koeffizienten Tabelle
  coef_list <- lapply(seq_along(models_to_report), function(i){
    m <- models_to_report[[i]]
    s <- summary(m)
    tibble(
      Block = paste0("Block ", i-1),
      Term = rownames(s$coefficients),
      Estimate = round(s$coefficients[, "Estimate"], 3),
      StdError = round(s$coefficients[, "Std. Error"], 3),
      t_value = round(s$coefficients[, "t value"], 3),
      p_value = round(s$coefficients[, "Pr(>|t|)"], 3)
    )
  })
  
  coef_df <- bind_rows(coef_list)
  
  cat("\n\n## Detailed Coefficients\n\n", file = md_file, append = TRUE)
  coef_df %>% kable(format = "markdown") %>% 
    cat(file = md_file, append = TRUE, sep = "\n")
}

# -------------------------
# Check Model Requirements mit implementierter Funktion
# -------------------------
for(name in names(model_list)){
  cat("\n--- Checking requirements for", name, "---\n")
  check_requirements(model_list[[name]])
}

# -------------------------
# Robuste Standardfehler für Block 1
# -------------------------


for(dv in info_indices){
  model <- model_list[[paste0(dv, "_1")]]
  cat("\nRobuste SE für:", dv, "\n")
  print(coeftest(model, vcov = vcovHC(model, type = "HC3")))
}


info_labels <- c(
  "Undirected Information Use",
  "Topic-related Information Use",
  "Problem-related Information Use",
  "Group-related Information Use"
)

html_file <- "03_Output/RQ3/All_Info_Models.html"

# Start HTML
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
  flex: 0 0 45%; /* Breite ~45% pro Tabelle */
  margin-bottom: 20px;
}
h2 {
  text-align: center;
}
</style>
</head>
<body>
<div class="container">
', file = html_file)

for(i in seq_along(info_indices)){
  dv <- info_indices[i]
  dv_label <- c(
    "Undirected Information Use",
    "Topic-related Information Use",
    "Problem-related Information Use",
    "Group-related Information Use"
  )[i]
  
  models_to_report <- list(
    model_list[[paste0(dv, "_0")]],
    model_list[[paste0(dv, "_1")]]
  )
  
  models_std <- lapply(models_to_report, lm.beta)
  
  # Tabellen-Container starten
  cat(paste0('<div class="table-box">\n<h2>', dv_label, '</h2>\n'), 
      file = html_file, append = TRUE)
  
  # Temp-Datei erstellen
  tmp <- tempfile(fileext = ".html")
  texreg::htmlreg(
    models_std,
    single.row = TRUE,
    digits = 2,
    custom.header = setNames(list(1:2), dv_label),
    custom.model.names = c("Block 0", "Block 1"),
    file = tmp
  )
  
  # Inhalt der temporären Datei in Master-Datei schreiben
  table_html <- readLines(tmp)
  table_html <- table_html[grep("<table", table_html):grep("</table>", table_html)]
  cat(paste0(table_html, collapse = "\n"), "\n", file = html_file, append = TRUE)
  unlink(tmp)
  
  # Tabellen-Container schließen
  cat('</div>\n', file = html_file, append = TRUE)
}

# Container und Body schließen
cat('</div>\n</body>\n</html>', file = html_file, append = TRUE)


# -------------------------
# End RQ3
# -------------------------







# Load fonts
font_import(paths = "C:/WINDOWS/Fonts", pattern = "times.ttf", prompt = FALSE)
loadfonts(device = "win")

# -------------------------
# 1. Correlation among substantial predictors
# -------------------------
# Korrelationsmatrix für die Prädiktoren der Modelle
df_corr <- df %>%
  select(
    age_years, 
    gender_binary, 
    education_cat, 
    platform_helper,
    idx_implicit,
    idx_explicit,
    idx_sociality,
    idx_incidentalness
  )

# Optional: numerische Variablen umwandeln (z.B. Faktoren)
df_corr <- df_corr %>%
  mutate(across(where(is.factor), as.numeric))

# Labels passend zu den Variablen
custom_labels <- c(
  "Age", 
  "Gender (female)", 
  "Education", 
  "Platform", 
  "Implicit Personalization", 
  "Explicit Personalization", 
  "Sociality", 
  "Incidentalness"
)

# Korrelation plotten
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

# Speichern
ggsave(
  filename = "04_Figures/RQ3/corrplot_model_predictors.png",
  plot = cplot,
  width = 2000, height = 2000, units = "px"
)


