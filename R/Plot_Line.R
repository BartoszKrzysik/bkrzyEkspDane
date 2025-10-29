#' Tworzy interaktywny wykres liniowy dla jednej kolumny
#'
#' Funkcja generuje wykres liniowy dla jednej kolumny numerycznej w ramce danych.
#' Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych), które są używane
#' do kolorowania linii. Opcjonalnie można dodać linię trendu (regresji liniowej) jako przerywaną linię.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do wizualizacji.
#' @param column Nazwa kolumny numerycznej do wizualizacji.
#' @param index Nazwa kolumny, która będzie używana jako oś X (np. czas). Jeśli NULL, użyje kolejnych numerów wierszy.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania linii (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @param abline Logical. Czy dodać linię trendu regresji liniowej (domyślnie TRUE).
#' @return Obiekt \pkg{plotly} z wykresem liniowym dla podanej kolumny.
#' @examples
#' plot_line(mtcars, column = "hp", index = "mpg", factor_cols = c("cyl"), abline = TRUE)
#' @export
plot_line <- function(df, column, index = NULL, factor_cols = NULL, abline = TRUE) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  if (!column %in% names(df_local)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df_local[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  if (is.null(index)) {
    df_local$Sample <- seq_len(nrow(df_local))
    index <- "Sample"
  } else if (!index %in% names(df_local)) {
    stop("Kolumna indeksowa ", index, " nie istnieje w df")
  }
  
  if (length(factor_cols) == 0 || identical(factor_cols, "All")) {
    p <- ggplot(df_local, aes_string(x = index, y = column)) +
      geom_line()
    if (abline) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = gg_color("red"),
                           linetype = "dashed", linewidth = 0.5)
    }
  } else {
    f <- factor_cols[1]
    p <- ggplot(df_local, aes_string(x = index, y = column, color = f)) +
      geom_line()
    if (abline) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5)
    }
  }
  
  p <- p + ggthemes::theme_clean(base_size = 15) +
    labs(x = index, y = column)
  
  return(plotly::ggplotly(p))
}
