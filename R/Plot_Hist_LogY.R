#' Tworzy interaktywny histogram z logarytmiczną skalą osi Y dla pojedynczej kolumny
#'
#' Funkcja generuje interaktywny histogram dla jednej kolumny numerycznej
#' w ramce danych z logarytmiczną skalą osi Y. Możliwe jest rozdzielenie
#' według kolumn jakościowych (faktorowych), które są używane do wypełnienia
#' kolorów i facetingu.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do wizualizacji.
#' @param column Nazwa kolumny numerycznej, dla której generowany jest histogram.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania i facetingu (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt \pkg{plotly} z histogramem dla podanej kolumny.
#' @examples
#' plot_hist_logY(mtcars, column = "mpg", factor_cols = c("cyl"))
#' @export
plot_hist_logY <- function(df, column, factor_cols = NULL) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  if (!column %in% names(df_local)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df_local[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  if (length(factor_cols) == 0 || identical(factor_cols, "All")) {
    p <- ggplot(df_local, aes_string(x = column)) +
      geom_histogram(color = "black", fill = gg_color("blue"), alpha = 0.7) +
      scale_y_log10() +
      ggthemes::theme_clean(base_size = 15) +
      xlab(column) +
      ylab("Count (log scale)")
  } else {
    f <- factor_cols[1]
    p <- ggplot(df_local, aes_string(x = column, fill = f)) +
      geom_histogram(color = "black", alpha = 0.7) +
      facet_wrap(as.formula(paste("~", f)), scales = "free") +
      scale_y_log10() +
      ggthemes::theme_clean(base_size = 15) +
      xlab(column) +
      ylab("Count (log scale)")
  }
  
  return(plotly::ggplotly(p))
}
