#' Tworzy siatkę histogramów z logarytmiczną skalą osi Y
#'
#' Funkcja generuje interaktywne histogramy dla wszystkich kolumn numerycznych
#' w ramce danych, z logarytmiczną skalą osi Y. Możliwe jest rozdzielenie
#' według kolumn jakościowych (faktorowych), które są używane do wypełnienia
#' kolorów i facetingu. Histogramy są zwracane jako lista subplotów
#' dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   i facetingu histogramów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @param bins Liczba koszyków (bins) w histogramach (domyślnie 20).
#' @return Obiekt klasy \code{tagList} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_hist_grid_logY(mtcars, factor_cols = c("cyl"), bins = 15)
#'
#' @export
plot_hist_grid_logY <- function(df, factor_cols = NULL, bins = 20) {
  if (is.null(factor_cols) || length(factor_cols) == 0) {
    df$All <- "All"
    factor_cols <- "All"
  }
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if(length(numeric_cols) == 0) {
    warning("Brak kolumn numerycznych do tworzenia histogramów (log Y).")
    return(NULL)
  }
  
  plots <- list()
  
  for (f in factor_cols) {
    for (n in numeric_cols) {
      p <- ggplot(df, aes_string(x = n, fill = f)) +
        geom_histogram(bins = bins, color = "black", alpha = 0.7) +
        facet_wrap(as.formula(paste("~", f)), scales = "free") +
        scale_y_log10() +
        theme_minimal(base_size = 15) +
        xlab(n) +
        ylab("Count (log scale)") +
        theme(legend.position = "none")
      
      plots <- c(plots, list(ggplotly(p) %>% layout(height = 400)))
    }
  }
  
  ncol_plot <- 2
  row_subplots <- list()
  for (i in seq(1, length(plots), by = ncol_plot)) {
    end_i <- min(i + ncol_plot - 1, length(plots))
    row_plots <- plots[i:end_i]
    row_subplots <- c(row_subplots, list(do.call(plotly::subplot, c(
      row_plots,
      list(nrows = 1, shareX = FALSE, shareY = FALSE, margin = 0.05)
    ))))
  }
  
  tagList(row_subplots)
}
