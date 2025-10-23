#' Tworzy siatkę wykresów punktowych (scatter) dla par kolumn numerycznych
#'
#' Funkcja generuje interaktywne wykresy punktowe dla wszystkich par kolumn
#' numerycznych w ramce danych. Możliwe jest rozdzielenie według kolumn jakościowych
#' (faktorowych), które są używane do kolorowania punktów. Wykresy są zwracane
#' jako lista subplotów dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   punktów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt klasy \code{tagList} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_scatter_grid(mtcars, factor_cols = c("cyl"))
#'
#' @export
plot_scatter_grid <- function(df, factor_cols = NULL) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if(length(numeric_cols) < 2) return(NULL)
  
  numeric_pairs <- combn(numeric_cols, 2, simplify = FALSE)
  plots <- list()
  
  for (f in factor_cols) {
    for (pair in numeric_pairs) {
      x_col <- pair[1]
      y_col <- pair[2]
      
      p <- ggplot(df_local, aes_string(x = x_col, y = y_col, color = f)) +
        geom_point(alpha = 0.7) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "none")
      
      plots <- c(plots, list(ggplotly(p) %>% layout(
        height = 400,
        xaxis = list(title = x_col),
        yaxis = list(title = y_col)
      )))
    }
  }
  
  ncol_plot <- 2
  row_subplots <- list()
  for (i in seq(1, length(plots), by = ncol_plot)) {
    end_i <- min(i + ncol_plot - 1, length(plots))
    row_plots <- plots[i:end_i]
    row_subplots <- c(row_subplots, list(do.call(subplot, c(
      row_plots,
      list(nrows = 1, shareX = FALSE, shareY = FALSE, margin = 0.05)
    ))))
  }
  
  tagList(row_subplots)
}
