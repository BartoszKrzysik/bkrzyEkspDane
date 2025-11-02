#' Generuje interaktywny raport danych w formacie HTML
#'
#' Funkcja tworzy raport z wybranej ramki danych (`data.frame`) w formacie HTML, wykorzystując
#' interaktywne tabele `reactable`. Wszystkie sekcje raportu są interaktywne:
#' tabele można sortować, filtrować, a dane numeric/factor/character mogą być filtrowane
#' przy pomocy suwaków i selektorów Crosstalk. Raport zawiera m.in.:
#' \itemize{
#'   \item wymiary danych,
#'   \item strukturę tabeli (`describe_df`),
#'   \item statystyki opisowe kolumn numerycznych (`describe_stats`),
#'   \item interaktywne wyświetlenie całych danych z filtrami,
#'   \item liczności kategorii dla kolumn tekstowych lub faktorowych (`reactable_counts`),
#'   \item informacje o sesji (`sessionInfo()`),
#'   \item stopkę z informacjami o pakiecie i autorze raportu.
#' }
#'
#' @param df Ramka danych (`data.frame` lub tibble), dla której ma zostać wygenerowany raport.
#' @param file Nazwa pliku wyjściowego HTML (domyślnie `"data_report.html"`).
#' @param title Tytuł raportu (domyślnie `"Raport danych"`).
#' @param author (Opcjonalnie) Imię i nazwisko autora raportu.
#' @param location (Opcjonalnie) Lokalizacja lub miejscowość, która ma się pojawić w raporcie.
#' @param open Logiczna wartość: jeśli TRUE, raport zostanie automatycznie otwarty w przeglądarce po wygenerowaniu (domyślnie FALSE).
#'
#' @return Zapisuje plik HTML w bieżącym katalogu roboczym i zwraca ścieżkę do pliku.  
#'   Jeśli `open = TRUE`, plik zostanie również otwarty w domyślnej przeglądarce.
#'
#' @details Funkcja konwertuje tibbles lub inne obiekty dziedziczące po `data.frame` na klasyczny `data.frame`,
#'   aby zapewnić pełną kompatybilność z pakietem `reactable`. Wszystkie tabele w raporcie są
#'   interaktywne i zachowują formatowanie z funkcji `describe_df()`, `describe_stats()` oraz `reactable_counts()`.
#'
#'   Sekcja danych umożliwia filtrowanie wartości numeric/factor/character przy użyciu suwaków i selektorów Crosstalk.
#'
#' @examples
#' \dontrun{
#' library(nycflights13)
#' # Raport podstawowy
#' data_report(flights)
#'
#' # Raport z autorem i lokalizacją, otwarty w przeglądarce
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
    "---\n\n"
  )
  
  setup_block <- paste0(
    "```{r setup, include=FALSE}\n",
    "library(dplyr)\n",
    "library(reactable)\n",
    "library(crosstalk)\n",
    "library(reactablefmtr)\n",
    "library(htmltools)\n",
    "knitr::opts_chunk$set(echo = FALSE)\n",
    "df <- as.data.frame(", deparse(substitute(df)), ")\n",
    "```\n\n"
  )
  style_block <- "
    <style>
    body {
      margin-left: 10px;
      margin-right: 10px;
    }
    .container {
      max-width: 100% !important;
    }
    </style>
    "
  dimension_block <- paste0(
    "## Wymiary danych\n\n",
    "```{r wymiar}\n",
    "reactable(\n",
    "  data.frame(Metryka = c('Liczba wierszy', 'Liczba kolumn'), Wartość = as.character(c(nrow(df), ncol(df)))),\n",
    "  defaultColDef = colDef(vAlign = 'center', align = 'left', width = 125),\n",
    "  striped = TRUE, pagination = TRUE, defaultPageSize = 15, highlight = TRUE, bordered = TRUE,\n",
    "  style = list(width = '253px', margin = '0 auto')\n",
    ")\n",
    "```\n\n"
  )
  
  str_block <- paste0(
    "# Struktura tabeli\n\n",
    "```{r str}\n",
    "reactable(describe_df(df),\n",
    "  defaultColDef = colDef(vAlign = 'center', align = 'left'),\n",
    "  columns = list(\n",
    "    col_index = colDef(sticky = 'left', name = 'Index', width = 60),\n",
    "    col_name = colDef(sticky = 'left', name = 'Nazwa kolumny', width = 150),\n",
    "    typeof = colDef(name = 'Typ', width = 100),\n",
    "    class = colDef(name = 'Klasa', width = 100),\n",
    "    n_unique = colDef(name = 'Unikalne', width = 100),\n",
    "    n_na = colDef(name = 'Ilość NaN', width = 100),\n",
    "    pct_na = colDef(name = 'Procent NaN', cell = function(value) paste0(value, '%'), width = 100),\n",
    "    n_zero = colDef(name = 'Ilość zer', width = 80),\n",
    "    pct_zero = colDef(name = 'Procent zer', cell = function(value) paste0(value, '%'), width = 100),\n",
    "    n_empty = colDef(name = 'Ilość pustych stringów', width = 80),\n",
    "    pct_empty = colDef(name = 'Procent pustych stringów', cell = function(value) paste0(value, '%'), width = 100),\n",
    "    n_null = colDef(name = 'Ilość null', width = 80),\n",
    "    pct_null = colDef(name = 'Procent null', cell = function(value) paste0(value, '%'), width = 100)\n",
    "  ),\n",
    "  striped = TRUE, pagination = TRUE, defaultPageSize = 15, highlight = TRUE, bordered = TRUE\n",
    ") %>% add_title('Struktura tabeli (str)', margin = margin(0, 0, 10, 0))\n",
    "```\n\n"
  )
  
  summary_block <- paste0(
    "# Statystyki tabeli\n\n",
    "```{r summary}\n",
    "reactable(describe_stats(df),\n",
    "  defaultColDef = colDef(vAlign = 'center', align = 'left'),\n",
    "  columns = list(\n",
    "    col_index = colDef(sticky = 'left', name = 'Index', width = 60),\n",
    "    col_name = colDef(sticky = 'left', name = 'Nazwa kolumny', width = 150),\n",
    "    mean = colDef(name = 'Średnia', width = 70),\n",
    "    mode = colDef(name = 'Moda', width = 70),\n",
    "    min = colDef(name = 'Minimum', width = 80),\n",
    "    Q1 = colDef(name = 'Q1', width = 65),\n",
    "    median = colDef(name = 'Median (Q2)', width = 75),\n",
    "    Q3 = colDef(name = 'Q3', width = 65),\n",
    "    max = colDef(name = 'Maksimum', width = 90),\n",
    "    sd = colDef(name = 'Odchylenie standardowe', width = 105),\n",
    "    IQR = colDef(name = 'IQR', width = 70)\n",
    "  ),\n",
    "  columnGroups = list(\n",
    "    colGroup(name = 'Kwantyle', columns = c('Q1', 'median', 'Q3')),\n",
    "    colGroup(name = 'Zakres', columns = c('min', 'max'))\n",
    "  ),\n",
    "  striped = TRUE, pagination = TRUE, defaultPageSize = 15, highlight = TRUE, bordered = TRUE\n",
    ") %>% add_title('Statystyki tabeli (summary)', margin = margin(0, 0, 10, 0))\n",
    "```\n\n"
  )
  
  data_block <- paste0(
    "# Dane interaktywne\n\n",
    "```{r data}\n",
    "dane <- SharedData$new(df)\n",
    "tbl <- reactable(dane,\n",
    "  defaultColDef = colDef(vAlign = 'center', align = 'left'),\n",
    "  striped = TRUE, pagination = TRUE, defaultPageSize = 15, highlight = TRUE, bordered = TRUE\n",
    ")\n",
    "div(bscols(c(\n",
    "  make_character_selects(dane, df),\n",
    "  make_numeric_sliders(dane, df),\n",
    "  make_factor_checkboxes(dane, df)\n",
    ")))\n",
    "div(tbl)\n",
    "```\n\n"
  )
  
  reactable_counts_block <- paste0(
    "# Liczności kategorii dla kolumn tekstowych\n\n",
    "```{r reactable_counts}\n",
    "reactable_counts(df)\n",
    "```\n"
  )
  
  session_block <- paste0(
    "# Session info\n\n",
    "```{r}\n",
    "sessionInfo()\n",
    "```\n"
  )
  
  footer_text <- paste0(
    "<hr>\n",
    "<p>Raport wygenerowany przez pakiet <a href='https://github.com/BartoszKrzysik/bkrzyEkspDane.git'>bkrzyEkspDane</a> na licencji MIT.</p>\n",
    "<p>Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.</p>\n"
  )
  
  rmd_text <- paste0(
    yaml_header,
    setup_block,
    style_block,
    dimension_block,
    str_block,
    summary_block,
    data_block,
    reactable_counts_block,
    session_block,
    footer_text
  )
  
  writeLines(rmd_text, tmp_rmd)
  
  output_path <- file.path(getwd(), file)
  rmarkdown::render(tmp_rmd, output_file = output_path, quiet = TRUE)
  
  message("Raport zapisany do pliku: ", output_path)
  
  if(open) browseURL(output_path)
}