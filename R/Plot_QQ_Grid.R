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
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if(length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  
  for (f in factor_cols) {
    for (n in numeric_cols) {
      p <- ggpubr::ggqqplot(df_local, x = n, color = f, palette = palette) +
        xlab(n) +
        ylab("Theoretical Quantiles") +
        ggthemes::theme_clean(base_size = 15)
      
      plots <- c(plots, list(ggplotly(p)))
    }
  }
  
  return(plots)
}
