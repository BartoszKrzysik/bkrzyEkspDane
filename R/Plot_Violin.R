#' Tworzy interaktywny wykres violin dla jednej kolumny numerycznej
#'
#' Funkcja generuje wykres violin dla jednej kolumny numerycznej.
#' Możliwe jest rozdzielenie według jednej kolumny jakościowej (faktorowej),
#' która jest używana do kolorowania i grupowania. Dodatkowo wyświetlany jest
#' boxplot dla wizualizacji mediany i kwartylów.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do wizualizacji.
#' @param numeric_col Nazwa kolumny numerycznej.
#' @param factor_col Nazwa kolumny jakościowej (faktorowej) do kolorowania i grupowania (domyślnie NULL).
#' @return Obiekt \pkg{plotly} z wykresem violin.
#' @examples
#' plot_violin(mtcars, numeric_col = "mpg", factor_col = "cyl")
#' @export
plot_violin <- function(df, numeric_col, factor_col = NULL) {
  
  if (!numeric_col %in% names(df)) stop("Kolumna ", numeric_col, " nie istnieje w df")
  if (!is.numeric(df[[numeric_col]])) stop("Kolumna ", numeric_col, " musi być numeryczna")
  if (!is.null(factor_col) && !factor_col %in% names(df)) {
    stop("Kolumna faktorowa ", factor_col, " nie istnieje w df")
  }
  
  x_aes <- ifelse(is.null(factor_col), "''", factor_col)
  
  p <- ggplot(df, aes_string(x = x_aes, y = numeric_col, fill = factor_col)) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white") +
    ggthemes::theme_clean(base_size = 15) +
    xlab(factor_col %||% "") +
    ylab(numeric_col)
  
  return(plotly::ggplotly(p))
}
