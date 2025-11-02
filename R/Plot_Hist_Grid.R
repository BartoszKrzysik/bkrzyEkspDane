#' Tworzy siatkę histogramów dla kolumn numerycznych
#'
#' Funkcja generuje interaktywne histogramy dla wszystkich kolumn numerycznych
#' w ramce danych. Możliwe jest rozdzielenie według kolumn jakościowych (faktorowych),
#' które są używane do wypełnienia kolorów i facetingu. Histogramy są zwracane
#' jako lista subplotów dla łatwego umieszczenia w RMarkdown.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do wizualizacji.
#' @param factor_cols Wektor nazw kolumn jakościowych (faktorowych) do kolorowania
#'   i facetingu histogramów (domyślnie NULL — wszystkie dane traktowane jako jedna grupa).
#' @return Obiekt klasy \code{list} z listą subplotów \pkg{plotly}.
#' @examples
#' plot_hist_grid(mtcars, factor_cols = c("cyl"))
#'
plot_hist_grid <- function(df, factor_cols = NULL, log_y = FALSE) {
  prep <- prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols

  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if (length(numeric_cols) == 0) return(NULL)

  plots <- list()

  for (f in factor_cols) {
    for (n in numeric_cols) {
      x_vals <- df_local[[n]]
      if (all(is.na(x_vals))) next

      binwidth <- compute_binwidth(x_vals)
      if (is.na(binwidth)) next

      df_bins <- compute_bins(x_vals, group = df_local[[f]], binwidth = binwidth)
      if (nrow(df_bins) == 0) next

      p <- ggplot2::ggplot(df_bins, ggplot2::aes(x = xmin, y = count, fill = fill, text = text)) +
        ggplot2::geom_col(color = "black", width = binwidth, alpha = 0.7) +
        ggplot2::facet_wrap(as.formula(paste("~", f)), scales = "free") +
        ggthemes::theme_clean(base_size = 15) +
        ggplot2::xlab(n) +
        ggplot2::ylab("Count")

      if (log_y) {
        p <- p + ggplot2::scale_y_log10()
      }

      plots[[paste(f, n, sep = "_")]] <- plotly::ggplotly(p, tooltip = "text")
    }
  }

  return(plots)
}