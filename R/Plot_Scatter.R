#' Tworzy interaktywny wykres punktowy (scatter) dla jednej pary kolumn numerycznych
#'
#' Funkcja generuje wykres punktowy (scatter) dla jednej pary kolumn numerycznych.
#' Możliwe jest rozdzielenie według jednej kolumny jakościowej (faktorowej), która
#' jest używana do kolorowania punktów.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param x_col Nazwa kolumny numerycznej dla osi X.
#' @param y_col Nazwa kolumny numerycznej dla osi Y.
#' @param factor_col Nazwa kolumny jakościowej (faktorowej) do kolorowania punktów (domyślnie NULL).
#' @return Obiekt \pkg{plotly} z wykresem punktowym.
#' @examples
#' plot_scatter(mtcars, x_col = "mpg", y_col = "disp", factor_col = "cyl")
#' @export
plot_scatter <- function(df, x_col, y_col, factor_col = NULL) {
  
  if (!x_col %in% names(df)) stop("Kolumna ", x_col, " nie istnieje w df")
  if (!y_col %in% names(df)) stop("Kolumna ", y_col, " nie istnieje w df")
  if (!is.numeric(df[[x_col]]) || !is.numeric(df[[y_col]])) {
    stop("Obie kolumny muszą być numeryczne")
  }
  if (!is.null(factor_col) && !factor_col %in% names(df)) {
    stop("Kolumna faktorowa ", factor_col, " nie istnieje w df")
  }
  
  p <- ggplot(df, aes_string(x = x_col, y = y_col, color = factor_col)) +
    geom_point(alpha = 0.7) +
    ggthemes::theme_clean(base_size = 15) +
    labs(x = x_col, y = y_col, color = factor_col)
  
  return(plotly::ggplotly(p))
}
