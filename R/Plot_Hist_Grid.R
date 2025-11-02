#' Tworzy siatkę histogramów dla kolumn numerycznych
#'
#' Funkcja generuje interaktywne histogramy dla wszystkich kolumn numerycznych
#' w ramce danych. Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych),
#' które są używane do wypełnienia kolorów i facetingu. Histogramy są zwracane
#' jako lista subplotów dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   i facetingu histogramów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt klasy \code{list} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_hist_grid(mtcars, factor_cols = c("cyl"))
#'
library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)

plot_hist_grid <- function(df, factor_cols = NULL) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if(length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  
  for (f in factor_cols) {
    for (n in numeric_cols) {
      x_vals <- na.omit(df_local[[n]])
      unique_vals <- length(unique(x_vals))
      
      if(unique_vals <= 30) {
        binwidth <- 1
      } else {
        iqr_val <- IQR(x_vals)
        n_obs <- length(x_vals)
        binwidth <- 2 * iqr_val / (n_obs)^(1/3)
        if(binwidth == 0 || is.na(binwidth)) {
          binwidth <- diff(range(x_vals)) / 30
        }
        decimal_prec <- decimal_precision(x_vals)
        binwidth <- signif(binwidth, decimal_prec)
      }
      
      df_bins <- ggplot_build(
        ggplot(df_local, aes_string(x = n, fill = f)) +
          geom_histogram(binwidth = binwidth)
      )$data[[1]]
      
      df_bins <- df_bins %>%
        mutate(
          range = paste0("[", signif(xmin, 4), " – ", signif(xmax, 4), ")"),
          text = paste0("Zakres: ", range, "<br>Ilość: ", count)
        )
      
      p <- ggplot(df_bins, aes(x = x, y = count, fill = fill, text = text)) +
        geom_col(color = "black", alpha = 0.7, width = binwidth) +
        facet_wrap(as.formula(paste("~", f)), scales = "free") +
        ggthemes::theme_clean(base_size = 15) +
        xlab(n) +
        ylab("Count")
      
      plots <- c(plots, list(ggplotly(p, tooltip = "text")))
    }
  }
  
  return(plots)
}

