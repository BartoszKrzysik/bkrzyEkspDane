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
plot_correlation_matrix <- function(df, method = "pearson", title = "Macierz korelacji") {
  
  df_num <- df[sapply(df, is.numeric)]
  if (ncol(df_num) < 2) {
    stop("Dane muszą zawierać co najmniej dwie kolumny numeryczne.")
  }
  
  M <- cor(df_num, method = method, use = "complete.obs")
  
  cols <- colorRampPalette(c("#619CFF", "#00BA38", "#F8766D"))(200)
  
  corrplot::corrplot(
    M,
    method = "color",
    type = "upper",
    tl.col = "black",
    addCoef.col = "black",
    number.cex = 0.8,
    col = rev(cols),
    tl.srt = 45,
    title = title,
    mar = c(0, 0, 2, 0)
  )
}
