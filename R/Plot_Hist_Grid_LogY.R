#' Tworzy siatkę histogramów z logarytmiczną skalą osi Y
#'
#' Funkcja generuje interaktywne histogramy dla wszystkich kolumn numerycznych
#' w ramce danych, z logarytmiczną skalą osi Y. Możliwe jest rozdzielenie
#' według kolumn jakościowych (faktorowych), które są używane do wypełnienia
#' kolorów i facetingu. Histogramy są zwracane jako lista subplotów
#' dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   i facetingu histogramów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt klasy \code{tagList} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_hist_grid_logY(mtcars, factor_cols = c("cyl"))
#'
plot_hist_grid_logY <- function(df, factor_cols = NULL) {
  df_local <- prepare_qualitative(df, factor_cols)$df
  factor_cols <- prepare_qualitative(df, factor_cols)$qualitative_cols

  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if (length(numeric_cols) == 0) return(NULL)

  plots <- list()

  for (f in factor_cols) {
    for (n in numeric_cols) {
      x_vals <- df_local[[n]]
      if (all(is.na(x_vals))) next

      binwidth <- compute_binwidth(x_vals)
      df_bins <- compute_bins(x_vals, group = df_local[[f]], binwidth = binwidth)

      p <- ggplot2::ggplot(df_bins, ggplot2::aes(x = xmin, y = count, fill = fill, text = text)) +
        ggplot2::geom_col(color = "black", alpha = 0.7, width = binwidth) +
        ggthemes::theme_clean(base_size = 15) +
        ggplot2::scale_y_log10() +
        ggplot2::xlab(n) +
        ggplot2::ylab("Count")

      if (length(unique(df_bins$fill)) > 1) {
        p <- p + ggplot2::facet_wrap(~fill, scales = "free")
      }

      plots <- c(plots, list(plotly::ggplotly(p, tooltip = "text")))
    }
  }

  return(plots)
}