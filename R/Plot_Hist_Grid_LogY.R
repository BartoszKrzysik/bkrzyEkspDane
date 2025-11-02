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
  prep <- bkrzyEkspDane:::prepare_qualitative(df, factor_cols)
  df_local <- prep$df
  factor_cols <- prep$qualitative_cols

  numeric_cols <- names(df_local)[sapply(df_local, is.numeric)]
  if (length(numeric_cols) == 0) return(NULL)

  plots <- list()

  for (f in factor_cols) {
    for (n in numeric_cols) {
      x_vals <- df_local[[n]]
      group_vals <- df_local[[f]]

      mask <- !is.na(x_vals)
      df_sub <- df_local[mask, , drop = FALSE]
      if (nrow(df_sub) == 0) next

      binwidth <- compute_binwidth(df_sub[[n]])

      p <- ggplot2::ggplot(df_sub, ggplot2::aes_string(x = n, fill = f)) +
        ggplot2::geom_histogram(
          binwidth = binwidth,
          color = "black",
          alpha = 0.7,
          ggplot2::aes(
            text = paste0(
              "Zakres: [", after_stat(xmin), " – ", after_stat(xmax), ")<br>",
              "Ilość: ", after_stat(count)
            )
          )
        ) +
        ggplot2::facet_wrap(as.formula(paste("~", f)), scales = "free") +
        ggthemes::theme_clean(base_size = 15) +
        ggplot2::scale_y_log10() +
        ggplot2::xlab(n) +
        ggplot2::ylab("Count")

      plots <- c(plots, list(plotly::ggplotly(p, tooltip = "text")))
    }
  }

  return(plots)
}
