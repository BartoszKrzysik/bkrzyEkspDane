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
  prep <- prepare_qualitative(df, qualitative_cols)
  df_local <- prep$df
  qualitative_cols <- prep$qualitative_cols
  
  num_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if (length(num_cols) < 2) stop("Potrzeba co najmniej dwóch kolumn numerycznych.")
  
  plots <- list()
  for (col in qualitative_cols) {
    p <- GGally::ggpairs(
      df_local,
      ggplot2::aes(color = .data[[col]]),
      columns = num_cols
    ) +
      ggthemes::theme_clean(base_size = 18) +
      ggplot2::ggtitle(paste("GGpairs – kolor wg:", col))+
      ggplot2::theme(aspect.ratio = 1)
    
    plots <- c(plots, list(ggplotly(p)))
  }
  
  return(plots)
}
