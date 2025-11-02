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
      x_vals <- x_vals[!is.na(x_vals)]
      if (length(x_vals) == 0) next

      unique_vals <- length(unique(x_vals))

      if (unique_vals <= 30) {
        binwidth <- 1
      } else {
        iqr_val <- IQR(x_vals)
        n_obs <- length(x_vals)
        binwidth <- 2 * iqr_val / (n_obs)^(1/3)
        if (binwidth <= 0 || is.na(binwidth)) binwidth <- diff(range(x_vals)) / 30
        binwidth <- signif(binwidth, digits = decimal_precision(x_vals) + 1)
      }

      breaks <- seq(min(x_vals), max(x_vals) + binwidth, by = binwidth)
      bin_index <- cut(x_vals, breaks = breaks, right = FALSE, include.lowest = TRUE)

      df_bins <- data.frame(
        x = x_vals,
        fill = df_local[[f]][!is.na(x_vals)],
        bin = bin_index
      )

      df_bins <- df_bins %>%
        dplyr::group_by(fill, bin) %>%
        dplyr::summarise(count = length(x), .groups = "drop") %>%
        dplyr::mutate(
          xmin = as.numeric(sub("\\[|,.*", "", bin)),
          xmax = as.numeric(sub(".*,|\\)", "", bin)),
          text = paste0("Zakres: [", xmin, " – ", xmax, ")<br>Ilość: ", count)
        )

      p <- ggplot2::ggplot(df_bins, ggplot2::aes(x = xmin, y = count, fill = fill, text = text)) +
        ggplot2::geom_col(color = "black", alpha = 0.7, width = binwidth) +
        ggplot2::facet_wrap(as.formula(paste("~", f)), scales = "free") +
        ggplot2::scale_y_log10() +
        ggthemes::theme_clean(base_size = 15) +
        ggplot2::xlab(n) +
        ggplot2::ylab("Count")

      plots <- c(plots, list(plotly::ggplotly(p, tooltip = "text")))
    }
  }

  return(plots)
}