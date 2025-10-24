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
        ggthemes::theme_clean(base_size = 15) +
        labs(x = x_col, y = y_col, color = f)
      
      plots <- c(plots, list(ggplotly(p)))
    }
  }
  
  return(plots)
}
