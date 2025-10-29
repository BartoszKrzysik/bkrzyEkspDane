#' Obsługuje zera w wybranych kolumnach
#'
#' Funkcja usuwa lub zamienia wartości 0 w wybranych kolumnach w ramce danych.
#'
#' @param df Ramka danych do przefiltrowania.
#' @param cols Wektor nazw kolumn, w których sprawdzane są wartości zerowe.
#' @param method Metoda obsługi zer: "omit" – usuwa wiersze z zerami,
#'   "NA" – zamienia zera na NA (domyślnie "omit").
#' @return Ramka danych po przetworzeniu.
#' @examples
#' df <- data.frame(a = c(1,0,3), b = c(0,2,3))
#' zero.handle(df, cols = c("a", "b"), method = "omit")
#' zero.handle(df, cols = c("a", "b"), method = "NA")
#' @export
zero.handle <- function(df, cols, method = c("omit", "NA")) {
  method <- match.arg(method)
  stopifnot(all(cols %in% names(df)))
  
  if (method == "omit") {
    df <- df %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ . != 0))
  } else if (method == "NA") {
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ dplyr::na_if(., 0)))
  }
  
  return(df)
}
