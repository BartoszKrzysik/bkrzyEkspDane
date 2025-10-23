#' Tworzy interaktywne macierze par (ggpairs) dla kolumn numerycznych
#'
#' Funkcja generuje wykresy macierzy par (\code{GGally::ggpairs}) dla wszystkich kolumn
#' numerycznych w ramce danych. Dla każdej kolumny jakościowej (faktorowej lub character)
#' tworzony jest osobny wykres, w którym kolor punktów odpowiada kategoriom danej zmiennej.
#' Wykresy są konwertowane na obiekty interaktywne \code{plotly} i zwracane jako lista
#' \code{tagList} gotowa do umieszczenia w RMarkdown lub shiny.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param qualitative_cols Wektor nazw kolumn jakościowych (faktorowych lub character),
#'   które mają być użyte do kolorowania punktów w wykresach. Domyślnie NULL — wszystkie dane
#'   traktowane są jako jedna grupa.
#'
#' @return Obiekt klasy \code{tagList} zawierający wykresy \code{ggpairs} jako obiekty
#'   interaktywne \pkg{plotly}, gotowe do wyrenderowania w HTML lub shiny.
#'
#' @examples
#' library(MASS)
#' plot_ggpairs(Cars93, qualitative_cols = c("Type", "Origin"))
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
    
    plots <- c(plots, list(plotly::ggplotly(p) %>% plotly::layout(height = 700)))
  }
  
  htmltools::tagList(plots)
}
