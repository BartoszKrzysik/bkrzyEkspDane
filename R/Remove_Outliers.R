#' Zastępuje wartości odstające `NA` w jednej lub wielu kolumnach
#'
#' Funkcja identyfikuje wartości odstające (outliers) w podanych kolumnach
#' numerycznych i zastępuje je `NA`. Pozwala zachować wszystkie wiersze ramki danych,
#' co jest przydatne w analizie szeregów czasowych.
#'
#' @param df Ramka danych zawierająca kolumny do oczyszczenia.
#' @param cols Wektor nazw kolumn numerycznych jako ciągi znaków.
#' @param method Metoda wykrywania outlierów: `"SD"` (domyślnie 3 sigma)
#'   lub `"IQR"` (1.5 * IQR). Wybór jest dokonywany automatycznie poprzez `match.arg`.
#'
#' @return Ramka danych z wartościami odstającymi zastąpionymi `NA`.
#' @examples
#' df <- data.frame(x = c(1,2,3,100,4,5), y = c(10,12,15,200,14,13))
#' remove_outliers(df, c("x","y"), method = "SD")
#' remove_outliers(df, c("x","y"), method = "IQR")
#' @export
remove_outliers <- function(df, cols, method = c("SD", "IQR")) {
  method <- match.arg(method)
  
  if (!all(cols %in% names(df))) {
    stop("Nie wszystkie podane kolumny istnieją w ramce danych")
  }
  
  for (col in cols) {
    if (!is.numeric(df[[col]])) stop(paste("Kolumna", col, "musi być numeryczna"))
    
    x <- df[[col]]
    
    if (method == "SD") {
      mu <- mean(x, na.rm = TRUE)
      sigma <- sd(x, na.rm = TRUE)
      outlier <- (x < mu - 3 * sigma) | (x > mu + 3 * sigma)
    } else if (method == "IQR") {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      outlier <- (x < lower) | (x > upper)
    }
    
    df[[col]][outlier] <- NA
  }
  
  return(df)
}
