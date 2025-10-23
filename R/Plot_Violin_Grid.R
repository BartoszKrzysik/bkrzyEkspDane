#' Tworzy siatkę wykresów violin dla kolumn numerycznych
#'
#' Funkcja generuje interaktywne wykresy violin dla wszystkich kolumn numerycznych
#' w ramce danych. Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych),
#' które są używane do kolorowania i grupowania. Każdy violin zawiera dodatkowo
#' wykres boxplot dla wizualizacji mediany i kwartylów. Wykresy są zwracane
#' jako lista subplotów dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   i grupowania wykresów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt klasy \code{tagList} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_violin_grid(mtcars, factor_cols = c("cyl"))
#'
#' @export
plot_violin_grid <- function(df, factor_cols = NULL) {
  
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if(length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  
  for (f in factor_cols) {
    for (n in numeric_cols) {
      p <- ggplot(df_local, aes_string(x = f, y = n, fill = f)) +
        geom_violin(trim = FALSE, alpha = 0.7) +
        geom_boxplot(width = 0.1, fill = "white") +
        theme_minimal(base_size = 15) +
        xlab(f) +
        ylab(n) +
        theme(legend.position = "none")
      
      plots <- c(plots, list(ggplotly(p) %>% layout(height = 400)))
    }
  }
  
  ncol_plot <- 2
  row_subplots <- list()
  
  for (i in seq(1, length(plots), by = ncol_plot)) {
    end_i <- min(i + ncol_plot - 1, length(plots))
    row_plots <- plots[i:end_i]
    row_subplots <- c(row_subplots, list(do.call(subplot, c(row_plots, 
                                                            list(nrows = 1, 
                                                                 shareX = FALSE, 
                                                                 shareY = FALSE, 
                                                                 margin = 0.05)))))
  }
  
  tagList(row_subplots)
}
