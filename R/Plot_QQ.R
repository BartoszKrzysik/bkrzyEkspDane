#' Tworzy interaktywny wykres Q-Q dla jednej kolumny
#'
#' Funkcja generuje wykres Q-Q (quantile-quantile) dla jednej kolumny numerycznej.
#' Możliwe jest rozdzielenie według jednej kolumny jakościowej (faktorowej), 
#' która jest używana do kolorowania punktów.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do wizualizacji.
#' @param column Nazwa kolumny numerycznej do analizy.
#' @param factor_col Nazwa kolumny jakościowej (faktorowej) do kolorowania (domyślnie NULL).
#' @return Obiekt \pkg{plotly} z wykresem Q-Q dla podanej kolumny.
#' @examples
#' plot_qq(mtcars, column = "mpg", factor_col = "cyl")
#' @export
plot_qq <- function(df, column, factor_col = NULL) {
  
  if (!column %in% names(df)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  if (!is.null(factor_col) && !factor_col %in% names(df)) {
    stop("Kolumna faktorowa ", factor_col, " nie istnieje w df")
  }
  
  p <- ggpubr::ggqqplot(df, x = column, color = factor_col) +
    xlab(column) +
    ylab("Theoretical Quantiles") +
    ggthemes::theme_clean(base_size = 15)
  
  return(plotly::ggplotly(p))
}
