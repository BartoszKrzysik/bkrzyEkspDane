#' Generuje raport danych w formacie HTML
#'
#' Funkcja tworzy raport z wybranej ramki danych (`data.frame`) w formacie HTML.
#' Raport zawiera m.in.:
#' \itemize{
#'   \item wymiary danych,
#'   \item nazwy i typy kolumn,
#'   \item brakujące wartości (NA) i liczby zer w kolumnach numerycznych,
#'   \item statystyki opisowe kolumn numerycznych,
#'   \item najczęstsze i najrzadsze wartości w kolumnach tekstowych lub faktorowych,
#'   \item korelacje między kolumnami numerycznymi,
#'   \item sessionInfo(),
#'   \item dodatkowe informacje o autorze i miejscowości (opcjonalnie).
#' }
#'
#' @param df Ramka danych (`data.frame` lub tibble), dla której ma zostać wygenerowany raport.
#' @param file Nazwa pliku wyjściowego HTML (domyślnie "data_report.html").
#' @param title Tytuł raportu (domyślnie "Raport danych").
#' @param author (Opcjonalnie) Imię i nazwisko autora raportu.
#' @param location (Opcjonalnie) Miejscowość lub lokalizacja, która ma się pojawić w raporcie.
#' @param open Logiczna wartość: jeśli TRUE, raport zostanie automatycznie otwarty w przeglądarce po wygenerowaniu (domyślnie FALSE).
#'
#' @return Zapisuje plik HTML w bieżącym katalogu roboczym i zwraca ścieżkę do pliku.
#'   Jeśli `open = TRUE`, plik zostanie również otwarty w domyślnej przeglądarce.
#'
#' @details Funkcja automatycznie konwertuje tibbles lub inne obiekty dziedziczące po `data.frame`
#'   na klasyczny `data.frame`, aby zapewnić pełną kompatybilność z pakietem `reactable`.
#'
#'   Raport generowany jest za pomocą `rmarkdown` i pakietu `reactable`, co umożliwia
#'   interaktywną prezentację tabel (sortowanie, filtrowanie).
#'
#' @examples
#' \dontrun{
#' library(nycflights13)
#' # Raport podstawowy
#' data_report(flights)
#'
#' # Raport z autorem i miejscowością, otwarty w przeglądarce
#' data_report(
#'   flights,
#'   title = "Raport o danych lotniczych",
#'   author = "John Doe",
#'   location = "Kraków",
#'   open = TRUE
#' )
#' }
#'
#' @export
data_report <- function(df, 
                        file = "data_report.html", 
                        title = "Raport danych", 
                        author = NULL,
                        location = NULL,
                        open = FALSE) {
  
  tmp_rmd <- tempfile(fileext = ".Rmd")
  after_html <- tempfile(fileext = ".html")
  
  footer_text <- paste0(
    "<hr>\n",
    "<p>Raport wygenerowany przez pakiet <a href='https://github.com/BartoszKrzysik/bkrzyEkspDane.git'>bkrzyEkspDane</a> na licencji MIT.</p>\n",
    "<p>Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.</p>\n"
  )
  writeLines(footer_text, after_html)
  
  yaml_header <- paste0(
    "---\n",
    "title: '", title, "'\n",
    if(!is.null(author)) paste0("author: '", author, "'\n") else "",
    if(!is.null(location)) paste0("location: '", location, "'\n") else "",
    "date: '`r format(Sys.Date(), \"%d %B %Y\")`'\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_depth: 3\n",
    "    toc_float: true\n",
    "    includes:\n",
    "      after_body: '", after_html, "'\n",
    "---\n\n"
  )
  rmd_text <- paste0(
    yaml_header,
    "```{r setup, include=FALSE}\n",
    "library(reactable)\n",
    "library(dplyr)\n",
    "knitr::opts_chunk$set(echo = FALSE)\n",
    "df <- ", deparse(substitute(df)), "\n",
    "df <- as.data.frame(df)\n",
    "```\n\n",
    
    "# Podstawowe dane\n\n",
    
    "## Wymiary danych\n\n",
    "```{r}\n",
    "reactable(data.frame(\n",
    "  Metryka = c('Liczba wierszy', 'Liczba kolumn'),\n",
    "  Wartość = as.character(c(nrow(df), ncol(df)))\n",
    "))\n",
    "```\n\n",
    
    "## Nazwy kolumn w danych\n\n",
    "```{r}\n",
    "reactable(data.frame(ID = seq_along(df), Kolumna = names(df)))\n",
    "```\n\n",
    
    "## Struktura danych `str()` \n\n",
    "```{r}\n",
    "str(df)\n",
    "```\n\n",
    
    "## Podsumowanie danych `summary()` \n\n",
    "```{r}\n",
    "summary(df)\n",
    "```\n\n",
    
    "# Potencjalne błędy\n\n",
    
    "## Braki danych (NA)\n\n",
    "```{r}\n",
    "na_count <- colSums(is.na(df))\n",
    "na_percent <- round(100 * na_count / nrow(df), 2)\n",
    "reactable(data.frame(\n",
    "  ID = seq_along(na_count),\n",
    "  Kolumna = names(na_count),\n",
    "  NA_count = as.integer(na_count),\n",
    "  NA_percent = na_percent\n",
    "))\n",
    "```\n\n",
    
    "## Liczba zer w kolumnach numerycznych\n\n",
    "```{r}\n",
    "zero_count <- sapply(df, function(x) if(is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA)\n",
    "zero_percent <- round(100 * zero_count / nrow(df), 2)\n",
    "reactable(data.frame(\n",
    "  ID = seq_along(zero_count),\n",
    "  Kolumna = names(zero_count),\n",
    "  Zero_count = as.integer(zero_count),\n",
    "  Zero_percent = zero_percent\n",
    "))\n",
    "```\n\n",
    
    "# Dane\n\n",
    
    "## Typy zmiennych\n\n",
    "```{r}\n",
    "reactable(data.frame(ID = seq_along(df), Kolumna = names(df), Typ = sapply(df, function(x) class(x)[1])))\n",
    "```\n\n",
    
    "## Liczba unikalnych wartości w kolumnach\n\n",
    "```{r}\n",
    "unique_count <- sapply(df, function(x) length(unique(x)))\n",
    "reactable(data.frame(ID = seq_along(unique_count), Kolumna = names(unique_count), Unique_count = unique_count))\n",
    "```\n\n",
    
    "## Najczęstsze wartości w kolumnach faktorowych lub character\n\n",
    "```{r}\n",
    "most_common <- lapply(seq_along(df), function(i) {\n",
    "  x <- df[[i]]\n",
    "  if(is.factor(x) || is.character(x)) {\n",
    "    tab <- sort(table(x), decreasing = TRUE)[1:min(3, length(unique(x)))]\n",
    "    data.frame(ID = i, Kolumna = names(df)[i], Wartość = names(tab), Liczność = as.integer(tab), stringsAsFactors = FALSE)\n",
    "  } else NULL\n",
    "}) %>% bind_rows()\n",
    "if(nrow(most_common) > 0) reactable(most_common) else 'Brak kolumn tekstowych lub faktorowych.'\n",
    "```\n\n",
    
    "## Najrzadsze wartości w kolumnach faktorowych lub character\n\n",
    "```{r}\n",
    "rarest_values <- lapply(seq_along(df), function(i) {\n",
    "  x <- df[[i]]\n",
    "  if(is.factor(x) || is.character(x)) {\n",
    "    tab <- sort(table(x), decreasing = FALSE)[1:min(3, length(unique(x)))]\n",
    "    data.frame(ID = i, Kolumna = names(df)[i], Wartość = names(tab), Liczność = as.integer(tab), stringsAsFactors = FALSE)\n",
    "  } else NULL\n",
    "}) %>% bind_rows()\n",
    "if(nrow(rarest_values) > 0) reactable(rarest_values) else 'Brak kolumn tekstowych lub faktorowych.'\n",
    "```\n\n",
    
    "## Podstawowe statystyki kolumn numerycznych\n\n",
    "```{r}\n",
    "num_stats <- df[, sapply(df, is.numeric), drop = FALSE]\n",
    "if(ncol(num_stats) > 0) {\n",
    "  summary_stats <- data.frame(\n",
    "    ID = seq_along(num_stats),\n",
    "    Kolumna = names(num_stats),\n",
    "    Mean = sapply(num_stats, mean, na.rm = TRUE),\n",
    "    Median = sapply(num_stats, median, na.rm = TRUE),\n",
    "    SD = sapply(num_stats, sd, na.rm = TRUE),\n",
    "    Min = sapply(num_stats, min, na.rm = TRUE),\n",
    "    Max = sapply(num_stats, max, na.rm = TRUE),\n",
    "    Q1 = sapply(num_stats, quantile, 0.25, na.rm = TRUE),\n",
    "    Q3 = sapply(num_stats, quantile, 0.75, na.rm = TRUE),\n",
    "    stringsAsFactors = FALSE\n",
    "  )\n",
    "  reactable(summary_stats)\n",
    "} else {\n",
    "  'Brak kolumn numerycznych w danych'\n",
    "}\n",
    "```\n\n",
    
    "## Korelacje między kolumnami numerycznymi\n\n",
    "```{r}\n",
    "if(ncol(num_stats) > 1) {\n",
    "  cor_mat <- cor(num_stats, use = 'pairwise.complete.obs')\n",
    "  reactable(cbind(ID = seq_len(nrow(cor_mat)), as.data.frame(cor_mat)), rownames = TRUE)\n",
    "} else {\n",
    "  'Brak wystarczającej liczby kolumn numerycznych do obliczenia korelacji.'\n",
    "}\n",
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
    "```\n",
    
    "# Session info\n\n",
    "```{r}\n",
    "sessionInfo()\n",
    "```\n"
  )
  
  writeLines(rmd_text, tmp_rmd)
  
  output_path <- file.path(getwd(), file)
  rmarkdown::render(tmp_rmd, output_file = output_path, quiet = TRUE)
  
  message("Raport zapisany do pliku: ", output_path)
  
  if (open) browseURL(output_path)
}