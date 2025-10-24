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
#' @param bins Liczba koszyków (bins) w histogramach (domyślnie 20).
#' @return Obiekt klasy \code{list} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_hist_grid(mtcars, factor_cols = c("cyl"), bins = 15)
#'
#' @export
plot_hist_grid <- function(df, factor_cols = NULL, bins = 20) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if(length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  for (f in factor_cols) {
    for (n in numeric_cols) {
      p <- ggplot(df_local, aes_string(x = n, fill = f)) +
        geom_histogram(bins = bins, color = "black", alpha = 0.7) +
        facet_wrap(as.formula(paste("~", f)), scales = "free") +
        ggthemes::theme_clean(base_size = 15) +
        xlab(n) +
        ylab("Count")
      
      plots <- c(plots, list(ggplotly(p)))
    }
  }
  
  return(plots)
}
