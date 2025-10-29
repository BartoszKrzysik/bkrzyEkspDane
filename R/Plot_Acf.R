#' Tworzy interaktywny wykres ACF dla pojedynczej kolumny
#'
#' Funkcja generuje wykres ACF (autocorrelation function) dla jednej kolumny
#' numerycznej w ramce danych. Wykres jest interaktywny dzięki pakietowi \pkg{plotly}.
#' Punkty istotne statystycznie (poza przedziałem ufności ±1.96/sqrt(n)) są wyróżnione innym kolorem.
#'
#' @param df Ramka danych zawierająca kolumnę do analizy.
#' @param column Nazwa kolumny numerycznej, dla której generujemy wykres ACF.
#' @param max_lag Maksymalna liczba lagów do policzenia ACF (domyślnie 20).
#' @return Obiekt \pkg{plotly} z wykresem ACF dla podanej kolumny.
#' @examples
#' plot_acf(mtcars, "mpg", max_lag = 15)
#' @export
plot_acf <- function(df, column, max_lag = 20) {
  
  if (!column %in% names(df)) stop("Kolumna ", column, " nie istnieje w df")
  if (!is.numeric(df[[column]])) stop("Kolumna ", column, " nie jest numeryczna")
  
  x <- df[[column]]
  acf_res <- acf(x, plot = FALSE, lag.max = max_lag, na.action = na.pass)
  
  n <- sum(!is.na(x))
  conf <- 1.96 / sqrt(n)
  
  acf_df <- data.frame(
    lag = acf_res$lag[,1,1],
    acf = acf_res$acf[,1,1]
  )
  
  acf_df$signif <- ifelse(acf_df$lag == 0, FALSE, abs(acf_df$acf) > conf)
  
  p <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_hline(yintercept = c(-conf, conf), linetype = "dashed", color = gg_color("red")) +
    geom_segment(aes(xend = lag, yend = 0, color = signif), size = 1) +
    scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE" = gg_color("green"))) +
    ggthemes::theme_clean(base_size = 15) +
    labs(title = paste("ACF:", column), x = "Lag", y = "ACF", color = "Istotny")
  
  return(plotly::ggplotly(p))
}
