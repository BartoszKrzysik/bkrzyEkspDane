#' Usuwa wiersze z zerami w wybranych kolumnach (dplyr)
#'
#' Funkcja usuwa wszystkie wiersze z ramki danych, w których w kolumnach
#' podanych w argumencie \code{cols} występuje wartość 0.
#'
#' @param df Ramka danych do przefiltrowania.
#' @param cols Wektor nazw kolumn, w których sprawdzane są wartości zerowe.
#' @return Ramka danych bez wierszy zawierających 0 w wybranych kolumnach.
#' @examples
#' df <- data.frame(a = c(1,0,3), b = c(0,2,3))
#' zero.omit(df, cols = c("a", "b"))
#'
#' @export
zero.omit <- function(df, cols) {
  stopifnot(all(cols %in% names(df)))
  
  df %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ . != 0))
}
