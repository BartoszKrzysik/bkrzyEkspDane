#' Tworzy raport HTML w formie flexdashboard z wizualizacjami danych
#'
#' Funkcja generuje automatyczny raport HTML w formie dashboardu, zawierający:
#' - macierze par (ggpairs),
#' - wykresy typu scatter, scatter z elipsą, histogram, density, violin, QQ,
#' - macierze korelacji (Pearson, Spearman, Kendall),
#' - informacje o sesji.
#' 
#' Wspiera wyróżnianie kolumn jakościowych (faktorowych lub character) poprzez kolorowanie.
#'
#' @param data Ramka danych do wizualizacji.
#' @param qualitative_cols Wektor nazw kolumn jakościowych, które mają być wyróżnione kolorami (domyślnie NULL).
#' @param file Nazwa pliku wyjściowego HTML (domyślnie "plots.html").
#' @param author Autor raportu (domyślnie NULL).
#' @param location Lokalizacja raportu (domyślnie NULL).
#' @param title Tytuł raportu (domyślnie "Data Visualization").
#' @param open Czy otworzyć raport w przeglądarce po wygenerowaniu (domyślnie FALSE).
#'
#' @return NULL. Funkcja zapisuje raport do pliku HTML i opcjonalnie otwiera go w przeglądarce.
#'
#' @examples
#' data_visualization(iris, qualitative_cols = "Species", author = "John Doe", location = "Kraków", open = TRUE)
#'
#' @export
data_visualization <- function(data, 
                               qualitative_cols = NULL, 
                               file = "plots.html",
                               author = NULL,
                               location = NULL,
                               title = "Data Visualization",
                               open = FALSE) {
  
  tmp_rmd <- tempfile(fileext = ".Rmd")
  yaml_header <- paste0(
    "---\n",
    "title: '", title, "'\n",
    if(!is.null(author)) paste0("author: '", author, "'\n") else "",
    if(!is.null(location)) paste0("location: '", location, "'\n") else "",
    "date: '`r format(Sys.Date(), \"%d %B %Y\")`'\n",
    "output:\n",
    "  flexdashboard::flex_dashboard:\n",
    "    orientation: columns\n",
    "    vertical_layout: scroll\n",
    "---\n\n"
  )
  
  custom_css <- "<style>
.panel { 
  height: 100%;
  min-height: 600px;
  overflow-y: auto;
  padding: 10px;
}
.column, .row {
  margin-bottom: 20px;
}
</style>\n\n"

# Treść Rmd
rmd_body <- c(
  custom_css,
  "```{r setup, include=FALSE}",
  "library(plotly)",
  "library(ggplot2)",
  "library(dplyr)",
  "library(flexdashboard)",
  "library(ggthemes)",
  "library(htmltools)",
  "```",
  "",
  "```{r load-data, include=FALSE}",
  "df <- data",
  if(!is.null(qualitative_cols)) paste0("factor_cols <- c(", paste0('"', qualitative_cols, '"', collapse = ", "), ")") else "factor_cols <- NULL",
  "```",
  "",
  "# GGpairs Plots {.column}",
  "",
  "## GGpairs Plots",
  "```{r ggpairs_plots, echo=FALSE, warning=FALSE, message=FALSE}",
  "plots_list <- plot_ggpairs(df, qualitative_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Scatter Ellipse Plots {.column}",
  "",
  "## Scatter Ellipse Plots",
  "```{r scatter_ellipse_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_scatter_ellipse(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Scatter Plots {.column}",
  "",
  "## Scatter Plots",
  "```{r scatter_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_scatter_grid(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Histogram (normal) {.column}",
  "",
  "## Histogram (normal)",
  "```{r histogram_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_hist_grid(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Histogram (log Y) {.column}",
  "",
  "## Histogram (log Y)",
  "```{r histogram_logY_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_hist_grid_logY(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Density plots {.column}",
  "",
  "## Density plots",
  "```{r density_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_density_grid(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Violin plots {.column}",
  "",
  "## Violin plots",
  "```{r violin_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_violin_grid(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# QQ plots {.column}",
  "",
  "## QQ plots",
  "```{r qq_plots, echo=FALSE, warning=FALSE, message=FALSE, fig.width=12, fig.height=8}",
  "plots_list <- plot_qq_grid(df, factor_cols = factor_cols)",
  "htmltools::tagList(plots_list)",
  "```",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Correlation plots",
  "",
  "### Pearson corplot",
  "```{r correlation_pearson,dev='png', echo=FALSE, warning=FALSE, message=FALSE,fig.width=8, fig.height=8}\n",
  "plot_correlation_matrix(df, 'pearson', 'Pearson Correlation')\n",
  "```\n",
  "",
  "### Spearman corplot",
  "```{r correlation_spearman,dev='png', echo=FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=8}\n",
  "plot_correlation_matrix(df, 'spearman', 'Spearman Correlation')\n",
  "```\n",
  "",
  "### Kendall corplot",
  "```{r correlation_kendall,dev='png', echo=FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=8}\n",
  "plot_correlation_matrix(df, 'kendall', 'Kendall Correlation')\n",
  "```\n",
  "### Footer",
  "Raport wygenerowany przez pakiet [bkrzyEkspDane](https://github.com/BartoszKrzysik/bkrzyEkspDane.git) na licencji MIT.\n",
  "Korzysta z pakietów open-source: ggthemes, ggplot2, plotly, ggpubr, dplyr, GGally, reactable, rmarkdown, flexdashboard.\n",
  "",
  "# Session info\n\n",
  "```{r session_info, echo=FALSE, warning=FALSE, message=FALSE, fig.height=12}\n",
  "sessionInfo()\n",
  "```\n"
)

writeLines(c(yaml_header, rmd_body), tmp_rmd)
output_path <- file.path(getwd(), file)
rmarkdown::render(tmp_rmd, output_file = output_path, quiet = TRUE)

if(open) browseURL(output_path)
}

