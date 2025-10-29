#' Tworzy interaktywny wykres PACF dla jednej kolumny
#'
#' Funkcja generuje wykres PACF (partial autocorrelation function) dla jednej
#' kolumny numerycznej w ramce danych. Punkty istotne statystycznie
#' (poza przedziałem ufności ±1.96/sqrt(n)) są wyróżnione innym kolorem.
#'
#' @param df Ramka danych zawierająca kolumnę numeryczną do analizy.
#' @param column Nazwa kolumny numerycznej do analizy.
#' @param max_lag Maksymalna liczba lagów do policzenia PACF (domyślnie 20).
#' @return Obiekt \pkg{plotly} z wykresem PACF dla podanej kolumny.
#' @examples
#' plot_pacf(mtcars, column = "mpg", max_lag = 15)
#' @export
plot_pacf <- function(df, column, max_lag = 20) {
  
  if (!column %in% names(df)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  x <- df[[column]]
  pacf_res <- pacf(x, plot = FALSE, lag.max = max_lag, na.action = na.pass)
  
  n <- sum(!is.na(x))
  conf <- 1.96 / sqrt(n)
  
  pacf_df <- data.frame(
    lag = pacf_res$lag,
    pacf = pacf_res$acf
  )
  
  pacf_df$signif <- abs(pacf_df$pacf) > conf
  
  p <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_hline(yintercept = c(-conf, conf), linetype = "dashed", color = gg_color("red")) +
    geom_segment(aes(xend = lag, yend = 0, color = signif), size = 1) +
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE" = gg_color("green"))) +
    ggthemes::theme_clean(base_size = 15) +
    labs(title = paste("PACF:", column), x = "Lag", y = "PACF", color = "Istotny")
  
  return(plotly::ggplotly(p))
}
