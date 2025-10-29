#' Tworzy interaktywny wykres gęstości (density) dla pojedynczej kolumny
#'
#' Funkcja generuje interaktywny wykres gęstości dla jednej kolumny
#' numerycznej w ramce danych. Możliwe jest rozdzielenie według kolumn
#' jakościowych (faktorowych), które są używane do wypełnienia kolorów.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do wizualizacji.
#' @param column Nazwa kolumny numerycznej, dla której generowany jest wykres.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt \pkg{plotly} z wykresem gęstości dla podanej kolumny.
#' @examples
#' plot_density(mtcars, column = "mpg", factor_cols = c("cyl"))
#' @export
plot_density <- function(df, column, factor_cols = NULL) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  if (!column %in% names(df_local)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df_local[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  if (length(factor_cols) == 0 || identical(factor_cols, "All")) {
    p <- ggplot(df_local, aes_string(x = column)) +
      geom_density(alpha = 0.5, fill = gg_color("blue")) +
      ggthemes::theme_clean(base_size = 15) +
      xlab(column) +
      ylab("Density")
  } else {
    f <- factor_cols[1]
    p <- ggplot(df_local, aes_string(x = column, fill = f)) +
      geom_density(alpha = 0.5) +
      ggthemes::theme_clean(base_size = 15) +
      xlab(column) +
      ylab("Density")
  }
  
  return(plotly::ggplotly(p))
}
