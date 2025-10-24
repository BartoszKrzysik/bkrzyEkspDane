#' Rysuje macierz korelacji dla kolumn numerycznych
#'
#' Funkcja generuje wizualizację macierzy korelacji dla wszystkich kolumn numerycznych
#' w ramce danych przy użyciu pakietu \pkg{corrplot}. Wyświetlane są wartości współczynników
#' oraz kolorowe pola odzwierciedlające siłę korelacji.
#'
#' @param df Ramka danych zawierająca kolumny numeryczne do analizy korelacji.
#' @param method Metoda obliczania korelacji: "pearson" (domyślnie), "spearman" lub "kendall".
#' @param title Tytuł wykresu (domyślnie "Macierz korelacji").
#' @return NULL. Funkcja generuje wykres jako efekt uboczny.
#' @examples
#' plot_correlation_matrix(mtcars, method = "pearson", title = "Korelacje mtcars")
#'
#' @export
plot_correlation_matrix<- function(df, method = "pearson", title = "Correlation Matrix") {
  df_num <- df[sapply(df, is.numeric)]
  if (ncol(df_num) < 2) stop("Dane muszą mieć co najmniej 2 kolumny numeryczne.")
  
  M <- cor(df_num, method = method, use = "complete.obs")
  
  df_long <- data.frame(
    Var1 = rep(rownames(M), times = ncol(M)),
    Var2 = rep(colnames(M), each = nrow(M)),
    Correlation = as.vector(M)
  )
  ggplot(df_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#619CFF", mid = "white", high = "#F8766D", midpoint = 0) +
    geom_text(aes(label = round(Correlation, 2)), size = 4) +
    theme_clean(base_size = 15) +
    labs(title = title, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

