#' Generuje raport HTML z eksploracji danych
#'
#' Funkcja tworzy automatyczny raport HTML z podstawową analizą ramki danych.
#' Raport zawiera m.in. wymiary danych, strukturę, brakujące wartości, liczbę zer,
#' typy zmiennych, liczbę unikalnych wartości, najczęstsze i najrzadsze wartości,
#' podstawowe statystyki kolumn numerycznych, korelacje oraz liczbę kategorii
#' w kolumnach faktorowych i znakowych.
#'
#' @param df Ramka danych do analizy.
#' @param file Nazwa pliku wyjściowego HTML (domyślnie "data_report.html").
#' @param title Tytuł raportu (domyślnie "Raport danych").
#' @param open Czy otworzyć raport w przeglądarce po wygenerowaniu (domyślnie FALSE).
#' @return NULL. Funkcja zapisuje raport do pliku i opcjonalnie otwiera go w przeglądarce.
#' @examples
#' data_report(mtcars, file = "mtcars_report.html", title = "Raport mtcars")
#'
#' @export
data_report <- function(df, file = "data_report.html", title = "Raport danych", open = FALSE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")
  
  tmp_rmd <- tempfile(fileext = ".Rmd")
  
  rmd_text <- paste0(
    "---\n",
    "title: '", title, "'\n",
    "output: html_document\n",
    "---\n\n",
    
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = TRUE)\n",
    "df <- ", deparse(substitute(df)), "\n",
    "```\n\n",
    
    "## Wymiary danych\n\n",
    "```{r}\n",
    "cat('Liczba wierszy:', nrow(df), '\\n')\n",
    "cat('Liczba kolumn:', ncol(df), '\\n')\n",
    "```\n\n",
    
    "## Struktura danych\n\n",
    "```{r}\n",
    "str(df)\n",
    "```\n\n",
    
    "## Podsumowanie danych\n\n",
    "```{r}\n",
    "summary(df)\n",
    "```\n\n",
    
    "## Braki danych (NA)\n\n",
    "```{r}\n",
    "na_count <- colSums(is.na(df))\n",
    "na_percent <- round(100 * na_count / nrow(df), 2)\n",
    "data.frame(NA_count = na_count, NA_percent = na_percent)\n",
    "```\n\n",
    
    "## Liczba zer w kolumnach numerycznych\n\n",
    "```{r}\n",
    "zero_count <- sapply(df, function(x) { if(is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA })\n",
    "zero_percent <- round(100 * zero_count / nrow(df), 2)\n",
    "data.frame(Zero_count = zero_count, Zero_percent = zero_percent)\n",
    "```\n\n",
    
    "## Typy zmiennych\n\n",
    "```{r}\n",
    "sapply(df, class)\n",
    "```\n\n",
    
    "## Liczba unikalnych wartości w kolumnach\n\n",
    "```{r}\n",
    "unique_count <- sapply(df, function(x) length(unique(x)))\n",
    "unique_count\n",
    "```\n\n",
    
    "## Najczęstsze wartości w kolumnach faktorowych lub character\n\n",
    "```{r}\n",
    "most_common <- lapply(df, function(x) {\n",
    "  if(is.factor(x) || is.character(x)) {\n",
    "    sort(table(x), decreasing = TRUE)[1:min(3, length(unique(x)))]\n",
    "  } else {\n",
    "    NA\n",
    "  }\n",
    "})\n",
    "most_common\n",
    "```\n\n",
    
    "## Najrzadsze wartości w kolumnach faktorowych lub character\n\n",
    "```{r}\n",
    "rarest_values <- lapply(df, function(x) {\n",
    "  if(is.factor(x) || is.character(x)) {\n",
    "    sort(table(x), decreasing = FALSE)[1:min(3, length(unique(x)))]\n",
    "  } else {\n",
    "    NA\n",
    "  }\n",
    "})\n",
    "rarest_values\n",
    "```\n\n",
    
    "## Podstawowe statystyki kolumn numerycznych\n\n",
    "```{r}\n",
    "num_stats <- df[, sapply(df, is.numeric), drop=FALSE]\n",
    "summary_stats <- data.frame(\n",
    "  Mean = sapply(num_stats, mean, na.rm=TRUE),\n",
    "  Median = sapply(num_stats, median, na.rm=TRUE),\n",
    "  SD = sapply(num_stats, sd, na.rm=TRUE),\n",
    "  Min = sapply(num_stats, min, na.rm=TRUE),\n",
    "  Max = sapply(num_stats, max, na.rm=TRUE),\n",
    "  Q1 = sapply(num_stats, quantile, 0.25, na.rm=TRUE),\n",
    "  Q3 = sapply(num_stats, quantile, 0.75, na.rm=TRUE)\n",
    ")\n",
    "summary_stats\n",
    "```\n\n",
    
    "## Korelacje między kolumnami numerycznymi\n\n",
    "```{r}\n",
    "cor(num_stats, use='pairwise.complete.obs')\n",
    "```\n\n",
    
    "## Liczności kategorii dla kolumn faktorowych\n\n",
    "```{r}\n",
    "factor_cols <- df[, sapply(df, is.factor), drop = FALSE]\n",
    "if(ncol(factor_cols) > 0) {\n",
    "  lapply(factor_cols, table)\n",
    "} else {\n",
    "  'Brak kolumn faktorowych w danych'\n",
    "}\n",
    "```\n\n",
    
    "## Liczności kategorii dla kolumn character\n\n",
    "```{r}\n",
    "char_cols <- df[, sapply(df, is.character), drop = FALSE]\n",
    "if(ncol(char_cols) > 0) {\n",
    "  lapply(char_cols, table)\n",
    "} else {\n",
    "  'Brak kolumn character w danych'\n",
    "}\n",
    "```\n"
  )
  
  writeLines(rmd_text, tmp_rmd)
  
  output_path <- file.path(getwd(), file)
  
  rmarkdown::render(tmp_rmd, output_file = output_path, quiet = TRUE)
  
  message("Raport zapisany do pliku: ", output_path)
  
  if(open) browseURL(output_path)
}
