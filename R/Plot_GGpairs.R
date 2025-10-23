#' Tworzy macierz par (pairplot) dla kolumn numerycznych z rozróżnieniem według zmiennych jakościowych
#'
#' Funkcja generuje wykresy macierzy par (\code{GGally::ggpairs}) dla wszystkich kolumn numerycznych
#' w ramce danych. Dla każdej kolumny jakościowej (\code{qualitative_cols}) tworzony jest osobny wykres,
#' w którym kolor punktów odpowiada kategoriom danej zmiennej.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param qualitative_cols Wektor nazw kolumn jakościowych (faktorowych) użytych do kolorowania wykresów.
#'   Domyślnie \code{NULL}, wtedy wszystkie dane traktowane są jako jedna grupa.
#'
#' @return Obiekt klasy \code{tagList} zawierający wykresy \code{ggpairs}, gotowy do renderowania w HTML/RMarkdown.
#'
#' @examples
#' plot_ggpairs(mtcars, qualitative_cols = c("cyl", "gear"))
#'
#' @export
plot_ggpairs <- function(df, qualitative_cols = NULL) {
  if (is.null(qualitative_cols) || length(qualitative_cols) == 0) {
    df$All <- "All"
    qualitative_cols <- "All"
  }
  
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) < 2) {
    stop("Potrzeba co najmniej dwóch kolumn numerycznych.")
  }
  
  plots <- list()
  
  for (col in qualitative_cols) {
    if (!col %in% names(df)) next
    
    if (!is.factor(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
    
    p <- GGally::ggpairs(
      df,
      ggplot2::aes(color = .data[[col]]),
      columns = num_cols
    ) +
      ggplot2::theme_minimal(base_size = 18) +
      ggplot2::ggtitle(paste("GGpairs – kolor wg:", col))
    
    plots <- c(plots, list(p))
  }
  
  htmltools::tagList(plots)
}
