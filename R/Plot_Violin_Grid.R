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
        ggthemes::theme_clean(base_size = 15) +
        xlab(f) +
        ylab(n) 
      
      plots <- c(plots, list(ggplotly(p)))
    }
  }
  
  return(plots)
}
