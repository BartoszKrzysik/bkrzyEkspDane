#' Tworzy interaktywne wykresy ACF dla wszystkich kolumn numerycznych
#'
#' Funkcja generuje wykresy ACF (autocorrelation function) dla wszystkich
#' kolumn numerycznych w ramce danych. Wykresy są interaktywne dzięki pakietowi \pkg{plotly}.
#' Punkty istotne statystycznie (poza przedziałem ufności ±1.96/sqrt(n)) są wyróżnione innym kolorem.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do analizy.
#' @param max_lag Maksymalna liczba lagów do policzenia ACF (domyślnie 20).
#' @return Lista obiektów \pkg{plotly} z wykresami ACF dla każdej kolumny numerycznej.
#' @examples
#' plot_acf_grid(mtcars, max_lag = 15)
#' 
plot_acf_grid <- function(df, max_lag = 20) {
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) == 0) return(NULL)
  
  plots <- list()
  
  for (col in numeric_cols) {
    x <- df[[col]]
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
      labs(title = paste("ACF:", col), x = "Lag", y = "ACF", color = "Istotny")
    
    plots <- c(plots, list(plotly::ggplotly(p)))
  }
  
  return(plots)
}
