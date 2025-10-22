#' Tworzy raport HTML z wizualizacjami danych
#'
#' Funkcja generuje automatyczny raport HTML z wizualizacjami danych,
#' obejmujący m.in. wykresy typu GGally ggpairs, scatter, histogram,
#' density, violin, QQ oraz macierze korelacji. Dodatkowo wspiera
#' wyróżnianie kolumn jakościowych (faktorowych) poprzez kolorowanie.
#'
#' @param data Ramka danych do wizualizacji.
#' @param qualitative_cols Wektor nazw kolumn jakościowych (faktorowych lub character),
#'   które mają być wyróżnione kolorami w wykresach (domyślnie NULL).
#' @param file Nazwa pliku wyjściowego HTML (domyślnie "plots.html").
#' @param open Czy otworzyć raport w przeglądarce po wygenerowaniu (domyślnie FALSE).
#' @param css_file Nazwa pliku CSS używanego w raporcie (domyślnie "data_visualization.css").
#' @return NULL. Funkcja zapisuje raport do pliku HTML i opcjonalnie otwiera go w przeglądarce.
#' @examples
#' data_visualization(mtcars, qualitative_cols = c("cyl", "gear"), file = "mtcars_plots.html")
#'
#' @export
data_visualization <- function(data, qualitative_cols = NULL, file = "plots.html", open = FALSE, css_file = "data_visualization.css") {
  
  tmp_rmd <- tempfile(fileext = ".Rmd")
  
  css_path <- system.file("css", css_file, package = "bkrzyEkspDane")
  css_link <- if (file.exists(css_path)) paste0("css: ", css_path) else ""
  
  rmd_text <- c(
    "---",
    "title: \"Data Visualization\"",
    "output:",
    "  html_document:",
    "    theme: null",
    if(css_link != "") paste0("    ", css_link),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(plotly)",
    "library(ggplot2)",
    "library(ggpubr)",
    "library(dplyr)",
    "library(GGally)",
    "library(htmltools)",
    "```",
    "",
    "```{r load-data, include=FALSE}",
    "df <- data",
    if(!is.null(qualitative_cols)) paste0("factor_cols <- c(", paste0('"', qualitative_cols, '"', collapse = ", "), ")") else "factor_cols <- NULL",
    "```",
    "",
    "## GGpairs plots",
    "```{r ggpairs, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=12}",
    "if (!is.null(factor_cols)) {",
    "  for (f in factor_cols) {",
    "    print(GGally::ggpairs(df, aes_string(color = f)) + theme_minimal(base_size = 18))",
    "  }",
    "} else {",
    "  print(GGally::ggpairs(df) + theme_minimal(base_size = 18))",
    "}",
    "```",
    "",
    "## Scatter plots",
    "```{r scatter, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_scatter_grid(df, factor_cols)",
    "```",
    "",
    "## Scatter with ellipse",
    "```{r scatter_ellipse, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_scatter_ellipse(df, factor_cols)",
    "```",
    "",
    "## Histogram (normal)",
    "```{r histogram_normal, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_hist_grid(df, factor_cols)",
    "```",
    "",
    "## Histogram (log Y)",
    "```{r histogram_logY, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_hist_grid_logY(df, factor_cols)",
    "```",
    "",
    "## Density plots",
    "```{r density, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_density_grid(df, factor_cols)",
    "```",
    "",
    "## Violin plots",
    "```{r violin, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_violin_grid(df, factor_cols)",
    "```",
    "",
    "## QQ plots",
    "```{r qq, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_qq_grid(df, factor_cols)",
    "```",
    "",
    "## Correlation plots",
    "```{r correlation, echo=FALSE, warning=FALSE, message=FALSE}",
    "plot_correlation_matrix(df, 'pearson', 'Pearson Correlation')",
    "plot_correlation_matrix(df, 'spearman', 'Spearman Correlation')",
    "plot_correlation_matrix(df, 'kendall', 'Kendall Correlation')",
    "```"
  )
  
  writeLines(rmd_text, tmp_rmd)
  output_path <- file.path(getwd(), file)
  rmarkdown::render(tmp_rmd, output_file = output_path, quiet = TRUE)
  if (open) browseURL(output_path)
}
