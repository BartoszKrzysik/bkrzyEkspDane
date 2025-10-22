#' Tworzy siatkę wykresów Q-Q dla kolumn numerycznych
#'
#' Funkcja generuje interaktywne wykresy Q-Q dla wszystkich kolumn numerycznych
#' w ramce danych. Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych),
#' które są używane do kolorowania. Wykresy są zwracane jako lista subplotów
#' dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   wykresów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @param palette Paleta kolorów dla grup faktorowych (domyślnie "jco").
#' @return Obiekt klasy \code{tagList} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_qq_grid(mtcars, factor_cols = c("cyl"), palette = "jco")
#'
#' @export
plot_qq_grid <- function(df, factor_cols = NULL, palette = "jco") {
  if (is.null(factor_cols) || length(factor_cols) == 0) {
    df$All <- "All"
    factor_cols <- "All"
  }
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if(length(numeric_cols) == 0) {
    warning("Brak kolumn numerycznych do tworzenia Q-Q plotów.")
    return(NULL)
  }
  
  plots <- list()
  
  for (f in factor_cols) {
    for (n in numeric_cols) {
      p <- ggpubr::ggqqplot(df, x = n, color = f, palette = palette) +
        xlab(n) +
        ylab("Theoretical Quantiles") +
        theme(legend.position = "none")
      
      plots <- c(plots, list(plotly::ggplotly(p) %>% layout(height = 400)))
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
