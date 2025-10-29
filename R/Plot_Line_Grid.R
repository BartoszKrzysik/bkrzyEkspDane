#' Tworzy interaktywne wykresy liniowe dla wszystkich kolumn numerycznych
#'
#' Funkcja generuje wykresy liniowe dla wszystkich kolumn numerycznych w ramce danych.
#' Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych), które są używane
#' do kolorowania linii. Opcjonalnie można dodać linię trendu (regresji liniowej) jako przerywaną linię.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param index Nazwa kolumny, która będzie używana jako oś X (np. czas). Jeśli NULL,
#'   funkcja użyje kolejnych numerów wierszy.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania linii
#'   (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @param abline Logical. Czy dodać linię trendu regresji liniowej (domyślnie TRUE).
#'
#' @return Lista obiektów \pkg{plotly} z wykresami liniowymi dla każdej kolumny numerycznej.
#' @examples
#' plot_line_grid(mtcars, index = "mpg", factor_cols = c("cyl"), abline = TRUE)
#' plot_line_grid(mtcars, abline = FALSE)
plot_line_grid <- function(df, index = NULL, factor_cols = NULL, abline = TRUE) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols
  
  if (is.null(index)) {
    df_local$Sample <- seq_len(nrow(df_local))
    index <- "Sample"
  } else if (!index %in% names(df_local)) {
    stop(paste("Kolumna indeksowa", index, "nie istnieje w ramce danych."))
  }
  
  numeric_cols <- setdiff(names(df_local)[sapply(df_local, is.numeric)], index)
  if (length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  
  for (col in numeric_cols) {
    for (f in factor_cols) {
      if (f == "All") {
        p <- ggplot(df_local, aes_string(x = index, y = col)) +
          geom_line()
        if (abline) {
          p <- p + geom_smooth(method = "lm", se = FALSE, color = gg_color("red"), 
                               linetype = "dashed", linewidth = 0.5)
        }
      } else {
        p <- ggplot(df_local, aes_string(x = index, y = col, color = f)) +
          geom_line()
        if (abline) {
          p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5)
        }
      }
      
      p <- p + ggthemes::theme_clean(base_size = 15) +
        labs(x = index, y = col)
      
      plots <- c(plots, list(plotly::ggplotly(p)))
    }
  }
  
  return(plots)
}
