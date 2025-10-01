# ============================================================
# Create Codebook from labelled data
# ============================================================

rm(list = ls())
gc()

pacman::p_load(dplyr, purrr, tibble, haven, stringr, readr)

# Funktion: Extrahiere Variablenlabel + Value Labels
extract_codebook <- function(data) {
  map_dfr(names(data), function(var) {
    var_data <- data[[var]]
    
    # Variablenlabel
    var_label <- attr(var_data, "label", exact = TRUE)
    if (is.null(var_label)) var_label <- ""
    
    # Value Labels
    val_labels <- attr(var_data, "labels", exact = TRUE)
    if (!is.null(val_labels)) {
      tibble(
        variable   = var,
        var_label  = var_label,
        value      = as.numeric(val_labels),
        value_label = names(val_labels)
      )
    } else {
      tibble(
        variable   = var,
        var_label  = var_label,
        value      = NA_real_,
        value_label = NA_character_
      )
    }
  })
}



make_codebook_md <- function(data, file = "CODEBOOK.md") {
  vars <- names(data)
  
  sections <- purrr::map_chr(vars, function(var) {
    x <- data[[var]]
    
    # Metadaten
    var_label <- attr(x, "label", exact = TRUE)
    if (is.null(var_label)) var_label <- ""
    val_labels <- attr(x, "labels", exact = TRUE)
    n_total <- length(x)
    n_missing <- sum(is.na(x))
    n_unique <- dplyr::n_distinct(x, na.rm = TRUE)
    typ <- paste(class(x), collapse = "/")
    
    # Falls es Value-Labels gibt
    if (!is.null(val_labels) && length(val_labels) > 0) {
      values <- as.numeric(val_labels)
      names_raw <- names(val_labels)
      if (is.null(names_raw)) names_raw <- rep(NA_character_, length(values))
      
      df <- tibble(
        value = values,
        value_label_raw = names_raw
      ) %>%
        arrange(value) %>%
        mutate(
          # Normalisieren / säubern
          value_label_raw = ifelse(is.na(value_label_raw), "", value_label_raw),
          value_label_clean = stringr::str_squish(value_label_raw),
          # entferne häufige Import-Artefakte: angehängte Ziffern (z.B. "Stimme zu1")
          value_label_clean = stringr::str_replace(value_label_clean, "\\s*\\d+$", ""),
          # Falls das Label jetzt leer ist oder identisch mit dem Code ("2"), dann als NA kennzeichnen
          value_label_clean = ifelse(value_label_clean == "" | value_label_clean == as.character(value),
                                     NA_character_, value_label_clean)
        )
      
      non_na <- sum(!is.na(df$value_label_clean))
      
      # Spezialformat: nur die Extrempunkte sind beschriftet
      if (non_na == 2 &&
          !is.na(df$value_label_clean[1]) &&
          !is.na(df$value_label_clean[nrow(df)])) {
        left_label  <- df$value_label_clean[1]
        right_label <- df$value_label_clean[nrow(df)]
        middle_codes <- if (nrow(df) > 2) paste(df$value[2:(nrow(df)-1)], collapse = ", ") else "(keine Zwischenwerte)"
        value_text <- paste0(
          "- ", df$value[1], " = ", left_label, " (linker Extrempunkt)\n",
          "- ", df$value[nrow(df)], " = ", right_label, " (rechter Extrempunkt)\n",
          "- Andere Codes: ", middle_codes, " (keine Beschriftungen)"
        )
      } else {
        # Normale Darstellung: jede Zeile + markiere fehlende Beschriftungen
        value_lines <- df %>%
          mutate(
            line = ifelse(is.na(value_label_clean),
                          paste0("- ", value, " = (keine Beschriftung)"),
                          paste0("- ", value, " = ", value_label_clean))
          ) %>%
          pull(line)
        value_text <- paste(value_lines, collapse = "\n")
      }
    } else {
      value_text <- "_(Keine Value Labels)_"
    }
    
    # Section zusammenbauen
    paste0(
      "## ", var, "\n\n",
      "**Frage/Label:** ", var_label, "\n\n",
      "**Typ:** ", typ, "\n\n",
      "**N:** ", n_total, " | **Missing:** ", n_missing, " | **Unique (non-missing):** ", n_unique, "\n\n",
      "**Value Labels:**\n\n",
      value_text,
      "\n"
    )
  })
  
  md_text <- paste0(
    "# Codebook\n\n",
    "Dieses Dokument wurde automatisch erzeugt. Variablen mit Value-Labels zeigen die Zuordnungen von Codes zu Texten.\n\n",
    paste0(sections, collapse = "\n\n")
  )
  
  readr::write_file(md_text, file)
  message("CODEBOOK geschrieben: ", file)
  
  # Optional: flache CSV mit allen Label-Paaren erzeugen (praktisch zum Durchsuchen)
  flat <- purrr::map_dfr(vars, function(var) {
    x <- data[[var]]
    var_label <- attr(x, "label", exact = TRUE)
    if (is.null(var_label)) var_label <- ""
    val_labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(val_labels) && length(val_labels) > 0) {
      vals <- as.numeric(val_labels)
      lbls <- names(val_labels)
      if (is.null(lbls)) lbls <- rep(NA_character_, length(vals))
      tibble(variable = var, var_label = var_label, value = vals, value_label = lbls)
    } else {
      tibble(variable = var, var_label = var_label, value = NA_real_, value_label = NA_character_)
    }
  })
  readr::write_csv(flat, "03_Output/CODEBOOK_flat.csv", na = "")
  message("Flache CSV geschrieben: CODEBOOK_flat.csv")
  
  invisible(list(md = md_text, flat = flat))
}


# Beispiel: Codebook für deinen Datensatz

clean_data <- readRDS(file = file.path("01_Data", "social_media_2025_clean_renamed.rds"))

codebook <- extract_codebook(clean_data)

# ------------------------------------------------------------
# Schön formatieren als Markdown
# ------------------------------------------------------------

# Markdown-Header
md_header <- "# Codebook\n\nDieses Dokument listet alle Variablen und deren Value Labels.\n\n"

# Gruppiert pro Variable als Markdown-Abschnitt
md_body <- codebook %>%
  group_by(variable, var_label) %>%
  summarise(
    values = paste0("- ", value, " = ", value_label, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(
    md_section = paste0(
      "## ", variable, "\n\n",
      "**Frage/Label:** ", var_label, "\n\n",
      ifelse(values == "- NA = NA", "_(Keine Value Labels)_", values),
      "\n"
    )
  ) %>%
  pull(md_section) %>%
  paste(collapse = "\n\n")

# Alles zusammenfügen
md_text <- paste0(md_header, md_body)

# ------------------------------------------------------------
# Export
# ------------------------------------------------------------
write_file(md_text, file = file.path("03_Output","CODEBOOK.md"))

make_codebook_md(clean_data, file = file.path("03_Output","CODEBOOK_v2.md"))
